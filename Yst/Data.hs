{-# LANGUAGE FlexibleContexts #-}
{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Yst.Data (getData, parseDataField)
where
import Yst.Types
import Yst.Util
import Yst.Yaml
import Yst.CSV
import Yst.Sqlite3 (readSqlite3)
import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import Data.List (sortBy, nub, isPrefixOf)
import Text.ParserCombinators.Parsec
import System.FilePath (takeExtension)

findData :: Site -> FilePath -> IO FilePath
findData = searchPath . dataDir

getData :: Site -> DataSpec -> IO Node
getData site (DataFromFile file opts) = do
  raw <- catch (findData site file >>= readDataFile)
          (\e -> errorExit 15 ("Error reading data from " ++ file ++ ": " ++ show e) >> return undefined)
  return $ foldl applyDataOption raw opts
getData site (DataFromSqlite3 database query opts) = do
  raw <- catch (findData site database >>= \d -> readSqlite3 d query)
          (\e -> errorExit 15 ("Error reading Sqlite3 database from " ++ database ++ ": " ++ show e) >> return undefined)
  return $ foldl applyDataOption raw opts
getData _ (DataConstant n) = return n

readDataFile :: FilePath -> IO Node
readDataFile f =
  case (map toLower $ takeExtension f) of
       ".yaml"      -> readYamlFile f
       ".csv"       -> readCSVFile f
       _            -> readYamlFile f

applyDataOption :: Node -> DataOption -> Node
applyDataOption (NList ns) (Limit lim) =
  NList $ take lim ns
applyDataOption (NList ns) (Where cond) =
  NList $ filter (satisfiesCond cond) ns
applyDataOption (NList ns) (GroupBy []) = NList ns
applyDataOption (NList ns) (GroupBy (k:ks)) =
  NList sorted
    where sorted             = map (\x -> applyDataOption (NList $ filter (\n -> getAttr k n == x) ns) (GroupBy ks)) keys
          keys               = nub $ map (getAttr k) ns
          getAttr x (NMap m) = lookup x m
          getAttr _ _        = error "Can't get an attribute from a non-map"
applyDataOption (NList ns) (OrderBy xs) =
  NList $ sortBy (compareNodeAt xs) ns
applyDataOption _ _ = error "order by and group by can be used only on lists"

satisfiesCond :: FilterCond -> Node -> Bool
satisfiesCond (And c1 c2) n = satisfiesCond c1 n && satisfiesCond c2 n
satisfiesCond (Or  c1 c2) n = satisfiesCond c1 n || satisfiesCond c2 n
satisfiesCond (Not c1)    n = not (satisfiesCond c1 n)
satisfiesCond (Filter test arg1 arg2) n =
  (filterTestPred test) (filterArgToNode arg1 n) (filterArgToNode arg2 n)

filterTestPred :: FilterTest -> (Node -> Node -> Bool)
filterTestPred TestEq    = (==)
filterTestPred TestGt    = (>)
filterTestPred TestLt    = (<)
filterTestPred TestGtEq  = (>=)
filterTestPred TestLtEq  = (<=)
filterTestPred TestContains = \n1 n2 -> case n1 of
                                          NList ns -> elem n2 ns
                                          _        -> False

filterArgToNode :: FilterArg -> Node -> Node
filterArgToNode (AttrValue attr) (NMap ns) = fromMaybe NNil (lookup attr ns)
filterArgToNode (AttrValue _) x = error $ "Cannot lookup attribute in non-map node: " ++ show x
filterArgToNode (StringConstant s) _ = NString s
filterArgToNode (DateConstant d) _ = NDate d

compareNodeAt :: [(String,SortDirection)] -> Node -> Node -> Ordering
compareNodeAt ((a,dir'):as) (NMap xs) (NMap ys) = reverseIfDescending dir' $
  case ((lookup a xs), (lookup a ys)) of
       (Just x, Just y)   -> case compare x y of
                                   EQ -> compareNodeAt as (NMap xs) (NMap ys)
                                   z  -> z
       (Just _, Nothing)  -> GT
       (Nothing, Just _)  -> LT
       (Nothing, Nothing) -> EQ
compareNodeAt [] (NMap _) (NMap _) = EQ
compareNodeAt _ _ _ = error "sortby and groupby can be used only on lists of maps"

reverseIfDescending :: SortDirection -> Ordering -> Ordering
reverseIfDescending Ascending o = o
reverseIfDescending Descending EQ = EQ
reverseIfDescending Descending LT = GT
reverseIfDescending Descending GT = LT

parseDataField :: Node -> DataSpec
parseDataField n@(NString s) = case parse pDataField s s of
  Right (f, Nothing, opts)  -> DataFromFile f opts
  Right (f, Just query, opts) -> DataFromSqlite3 f query opts
  Left err        -> if "from" `isPrefixOf` (dropWhile isSpace $ map toLower s)
                        then error $ "Error parsing data field: " ++ show err
                        else DataConstant n 
parseDataField n = DataConstant n

pDataField :: GenParser Char st (String, Maybe String,[DataOption])
pDataField = do
  spaces
  pString "from"
  pSpace
  fname <- pIdentifier <?> "name of YAML, CSV or SQLite3 file"
  query <- (optionMaybe $ pQuery) <?> "a SQL query"
  opts <- many $ (pOptWhere <?> "where [CONDITION]")
              <|> (pOptLimit <?> "limit [NUMBER]")
              <|> (pOptOrderBy <?> "order by [CONDITION]")
              <|> (pOptGroupBy <?> "group by [CONDITION]")
  spaces
  optional $ char ';'
  spaces
  eof
  return (fname, query, opts)

pIdentifier :: GenParser Char st [Char]
pIdentifier = spaces >> (pQuoted '\'' <|> pQuoted '"' <|> many (noneOf " \t\n<>=;,'\""))

-- | Case-insensitive string parser.
pString :: String -> GenParser Char st String
pString s = do
  s' <- count (length s) anyChar
  if map toLower s == map toLower s'
     then return s
     else mzero

pQuoted :: Char -> GenParser Char st [Char]
pQuoted delim = try $ do
  char delim
  res <- many (noneOf [delim] <|> (try $ char '\\' >> char delim))
  char delim
  return res

pQuery :: GenParser Char st String
pQuery = try $ do
  optional $ oneOf ",;"
  spaces
  pString "query"
  pSpace
  res <- pQuoted '"'
  return res

pOptLimit :: GenParser Char st DataOption
pOptLimit = try $ do
  optional $ oneOf ",;"
  spaces
  pString "limit"
  pSpace
  lim <- many1 digit
  return $ Limit $ read lim

pOptOrderBy :: GenParser Char st DataOption
pOptOrderBy = try $ do
  optional $ oneOf ",;"
  spaces
  pString "order"
  pSpace
  pString "by"
  pSpace
  keys <- spaces >> sepBy1 pSortKey (try $ pSpace >> spaces >> pString "then" >> pSpace)
  return $ OrderBy keys

pSortKey :: GenParser Char st ([Char], SortDirection)
pSortKey = do
  res <- pIdentifier
  dir' <- option Ascending pAscDesc
  return (res, dir')

pAscDesc :: GenParser Char st SortDirection
pAscDesc = (try $ pSpace >> pString "desc" >> return Descending)
       <|> (try $ pSpace >> pString "asc" >> return Ascending)

pOptGroupBy :: GenParser Char st DataOption
pOptGroupBy = try $ do
  optional $ oneOf ",;"
  spaces
  pString "group"
  pSpace 
  pString "by"
  pSpace
  keys <- spaces >> sepBy1 pIdentifier (try $ pSpace >> spaces >> pString "then" >> pSpace)
  return $ GroupBy keys

pOptWhere :: GenParser Char st DataOption
pOptWhere = try $ do
  optional $ oneOf ",;"
  spaces
  pString "where"
  pSpace
  cond <- pBooleanCondition
  return $ Where cond

pBooleanCondition :: GenParser Char st FilterCond
pBooleanCondition = spaces >> (pNot <|> pAnd <|> pOr <|> pInParens pBooleanCondition <|> pBasicCond)

pInParens :: GenParser Char st a -> GenParser Char st a
pInParens innerParser = try $ do
  char '('
  spaces
  res <- innerParser
  spaces
  char ')'
  return res 

pNot :: GenParser Char st FilterCond
pNot = try $ pString "not" >> pSpace >> liftM Not pBooleanCondition

pAnd :: GenParser Char st FilterCond
pAnd = try $ do
  x <- pNot <|> pInParens pBooleanCondition <|> pBasicCond
  pSpace
  pString "and"
  pSpace
  y <- pBooleanCondition
  return $ And x y

pOr :: GenParser Char st FilterCond
pOr = try $ do
  x <- pNot <|> pAnd <|> pInParens pBooleanCondition <|> pBasicCond
  pSpace
  pString "or"
  pSpace
  y <- pBooleanCondition
  return $ Or x y

pBasicCond :: GenParser Char st FilterCond
pBasicCond = try $ do
  x <- pFilterArg
  t <- pFilterTest
  y <- pFilterArg
  return $ Filter t x y

pFilterArg :: GenParser Char st FilterArg
pFilterArg = spaces >> (pStringOrDateConstant <|> pAttrValue)

pStringOrDateConstant :: GenParser Char st FilterArg
pStringOrDateConstant = do
  str <- pQuoted '"' <|> pQuoted '\''
  case parseAsDate str of
       Just d    -> return $ DateConstant d
       Nothing   -> return $ StringConstant str

pAttrValue :: GenParser Char st FilterArg
pAttrValue = liftM AttrValue pIdentifier

pFilterTest :: GenParser Char st FilterTest
pFilterTest = do
  spaces
  choice $ map (\(s,t) -> try $ string s >> return t)
         [ ("==",TestEq)
         , ("=",TestEq)
         , (">=",TestGtEq)
         , ("<=",TestLtEq)
         , (">",TestGt)
         , ("<",TestLt)
         , ("contains",TestContains)
         ]

pSpace :: GenParser Char st ()
pSpace = oneOf " \t\n" >> spaces


