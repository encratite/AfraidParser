import Control.Monad
import qualified Data.ByteString.Lazy as DBL
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import System.Environment

import Knyaz.Directory

main :: IO ()
main = do
  arguments <- getArgs
  if length arguments == 1
    then do results <- processDirectory $ head arguments
            return ()
    else putStrLn "Invalid argument count"

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