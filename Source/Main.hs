import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy as DBL
import Text.Parsec
import System.Environment

import Knyaz.Directory

import Algorithm
import Parser

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

writeOutput :: String -> FilePath -> String -> IO ()
writeOutput description path content =
  catch (do writeFile path content
            putStrLn $ "Output of " ++ outputDescription ++ " has been written to " ++ path)
        (\exception -> putStrLn $ "Failed to write the output of " ++ outputDescription ++ " to " ++ path ++ ": " ++ show exception)
  where
    quotify text = "\"" ++ text ++ "\""
    outputDescription = quotify description