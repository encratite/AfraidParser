import qualified Data.ByteString.Lazy as DBL
import System.Environment
import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Combinator

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
            case parse markup "domain" domainParser of
              Left error -> do putStrLn $ "Parser error: " ++ show error
                               return Nothing
              Right strings -> do return $ Just strings
            return Nothing)
        (\exception -> do putStrLn $ "Unable to read file " ++ path ++ ": " ++ show exception
                          return Nothing)
  where
    path = filePath information

domainParser :: Parser [String]
domainParser = do
  return . many $ do
    string "edit_domain_id="
    many $ noneOf ">"
    return . many $ noneOf "<"