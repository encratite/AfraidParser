module Parser(
  domainParser,
  tldParser
  ) where

import Control.Monad
import Text.Parsec
import Text.Parsec.ByteString.Lazy

domainParser :: Parser [String]
domainParser = do
  many . try $ do
    void $ manyTill anyChar $ try $ string "edit_domain_id="
    void . many $ noneOf ">"
    void $ string ">"
    many $ noneOf "<"

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