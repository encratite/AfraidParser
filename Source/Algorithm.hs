module Algorithm(
  DomainAlgorithm,
  DomainParserException,
  domainsSortedByLength,
  domainsSortedByTLDRarity,
  exceptionDomain,
  exceptionParserError
  ) where

import qualified Control.Exception as CE
import Data.Foldable (toList)
import Data.Function
import Data.List
import qualified Data.Map as DM
import qualified Data.Sequence as DS
import Data.Typeable
import Text.Parsec

import Parser

type DomainAlgorithm = [String] -> String

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