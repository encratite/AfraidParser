import qualified Control.Exception as CE
import Control.Monad
import qualified Data.ByteString.Lazy as DBL
import Data.Foldable (toList)
import Data.Function
import Data.List
import qualified Data.Map as DM
import qualified Data.Sequence as DS
import Data.Typeable
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import System.Environment

import Knyaz.Directory

type DomainAlgorithm = [String] -> String

data DomainAlgorithmDescription = DomainAlgorithmDescription {
  algorithmFunction :: DomainAlgorithm,
  algorithmDescription :: String,
  algorithmOutputPath :: FilePath
  }

main :: IO ()
main = do
  arguments <- getArgs
  if length arguments == length argumentDescriptions
    then do let inputDirectory = head arguments
                algorithm function description index = DomainAlgorithmDescription function description $ arguments !! index
                algorithms = [
                  algorithm domainsSortedByLength "domains sorted by length" 1,
                  algorithm domainsSortedByTLDRarity "domains sorted by TLD rarity" 2
                  ]
            results <- processDirectory inputDirectory
            case results of
              Just domains -> mapM_ (processAlgorithm domains) algorithms
              Nothing -> return ()
    else do program <- getProgName
            let description = [
                  "Invalid argument count. Usage:",
                  program ++ " " ++ argumentString
                  ]
            putStr $ unlines description
  where
    argumentDescriptions = [
      "input directory which contains the HTML files",
      "output path for the domains sorted by lenght",
      "output path for the domains sorted by TLD rarity"
      ]
    argumentString = unwords $ map (\x -> "<" ++ x ++ ">") argumentDescriptions
    processAlgorithm domains algorithm = do
      let content = algorithmFunction algorithm $ domains
      CE.catch (writeOutput (algorithmDescription algorithm) (algorithmOutputPath algorithm) content)
               handleDomainException

handleDomainException :: DomainParserException -> IO ()
handleDomainException exception =
  putStrLn $ "Domain parsing error in domain " ++ show (exceptionDomain exception) ++ ": " ++ show (exceptionParserError exception)

processDirectory :: FilePath -> IO (Maybe [String])
processDirectory directory = do
  result <- readDirectory directory
  case result of
    Just entries -> do
      contents <- mapM processFile entries
      return . Just $ foldl contentHandler [] contents
    _ -> do
      putStrLn $ "Unable to process directory " ++ directory
      return Nothing
  where contentHandler list fileResults =
          list ++ case fileResults of
            Just results -> results
            _ -> []

processFile :: FileInformation -> IO (Maybe [String])
processFile information =
  catch (do markup <- DBL.readFile path
            putStrLn $ "Read " ++ show (DBL.length markup) ++ " bytes from " ++ path
            let parserResult = parse domainParser "domain" markup
            case parserResult of
              Left parserError -> do putStrLn $ "Parser error: " ++ show parserError
                                     return Nothing
              Right strings -> do return $ Just strings)
        (\exception -> do putStrLn $ "Unable to read file " ++ path ++ ": " ++ show exception
                          return Nothing)
  where
    path = filePath information

domainParser :: Parser [String]
domainParser = do
  many . try $ do
    void $ manyTill anyChar $ try $ string "edit_domain_id="
    void . many $ noneOf ">"
    void $ string ">"
    many $ noneOf "<"

writeOutput :: String -> FilePath -> String -> IO ()
writeOutput description path content =
  catch (do writeFile path content
            putStrLn $ "Output of " ++ outputDescription ++ " has been written to " ++ path)
        (\exception -> putStrLn $ "Failed to write the output of " ++ outputDescription ++ " to " ++ path ++ ": " ++ show exception)
  where
    quotify text = "\"" ++ text ++ "\""
    outputDescription = quotify description

domainsSortedByLength :: DomainAlgorithm
domainsSortedByLength domains =
  let sortedDomains = sortBy domainSort domains in
  unlines sortedDomains
  where
    domainSort x y =
      let lengthComparison = on compare length x y in
      case lengthComparison of
        EQ -> compare x y
        _ -> lengthComparison

domainsSortedByTLDRarity :: DomainAlgorithm
domainsSortedByTLDRarity domains =
  output
  where
    tldMap = createTLDMap domains DM.empty
    sortedTLDMap = DM.map DS.unstableSort tldMap
    mapPairs = DM.assocs sortedTLDMap
    pairSort = on compare $ DS.length . snd
    sortedPairs = sortBy pairSort mapPairs
    pairMap (tld, domainSequence) = tld ++ " (" ++ (show $ DS.length domainSequence) ++ " domain(s))\n" ++ (unlines $ toList domainSequence)
    output = unlines $ map pairMap sortedPairs

type DomainMap = DM.Map String (DS.Seq String)

data DomainParserException = DomainParserException {
  exceptionDomain :: String,
  exceptionParserError :: ParseError
  } deriving (Show, Typeable)

instance CE.Exception DomainParserException

createTLDMap :: [String] -> DomainMap -> DomainMap
createTLDMap [] tldMap = tldMap
createTLDMap (domain : domains) tldMap =
  case parse tldParser "TLD" domain of
    Right tld -> let newMap = DM.insertWith' (DS.><) tld (DS.singleton domain) tldMap in
      createTLDMap domains newMap
    Left parserError -> CE.throw $ DomainParserException domain parserError

tldParser :: Parsec String () String
tldParser =
  try (do
    void $ char '.'
    tld <- many1 lower
    eof
    return tld) <|>
    (do
       void $ anyChar
       tldParser)