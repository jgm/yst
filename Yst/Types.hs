{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
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

module Yst.Types
where
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.Time
import qualified Data.Text as T
import Text.StringTemplate
import Data.Aeson
import qualified Data.Map as M
import System.Locale (defaultTimeLocale)
import Control.Monad

data Site = Site {
    siteTitle     :: String
  , sourceDir     :: [FilePath]
  , dataDir       :: [FilePath]
  , filesDir      :: [FilePath]
  , deployDir     :: FilePath
  , defaultLayout :: FilePath
  , indexFile     :: FilePath
  , pageIndex     :: M.Map String Page
  , navigation    :: [NavNode]
  } deriving (Show, Read, Eq)

data Source = TemplateFile FilePath
            | SourceFile FilePath
            deriving (Show, Read, Eq)

data Page = Page {
    pageData      :: [(String, DataSpec)]
  , layoutFile    :: Maybe FilePath
  , sourceFile    :: Source
  , requiresFiles :: [FilePath]
  , pageUrl       :: String
  , pageTitle     :: String
  , pageInMenu    :: Bool
  } deriving (Show, Read, Eq)

data DataSpec = DataConstant Node
              | DataFromFile FilePath [DataOption]
              | DataFromSqlite3 FilePath String [DataOption]
              deriving (Show, Read, Eq)

data DataOption = OrderBy [(String, SortDirection)]
                | GroupBy [String]
                | Where FilterCond
                | Limit Int 
                deriving (Show, Read, Eq)

data FilterCond = Filter FilterTest FilterArg FilterArg
                | And FilterCond FilterCond
                | Or  FilterCond FilterCond
                | Not FilterCond
                | Has String
                deriving (Show, Read, Eq)

data FilterArg = AttrValue String
               | StringConstant String
               | DateConstant Day
               deriving (Show, Read, Eq) 

data FilterTest = TestEq
                | TestGt
                | TestLt
                | TestGtEq
                | TestLtEq
                | TestContains
                deriving (Show, Read, Eq)

data NavNode = NavPage String String
             | NavMenu String [NavNode]
             deriving (Show, Read, Eq)

data Node = NString String
          | NDate Day
          | NList [Node]
          | NMap [(String, Node)]
          | NNil
          deriving (Show, Read, Eq)

instance Ord Node where
  compare (NString x) (NString y)   = compare x y
  compare (NDate x) (NDate y)       = compare x y
  compare (NList x) (NList y)       = compare x y
  compare (NMap x) (NMap y)         = compare x y
  compare NNil NNil                 = EQ
  compare (NList x) y               = compare (NList x) (NList [y])
  compare x (NList y)               = compare (NList [x]) (NList y)
  compare (NString _) _             = GT
  compare (NDate _) _               = GT
  compare _ _                       = GT

instance FromJSON Node where
  parseJSON (String t) = do
    let t' = T.unpack t
    case parseAsDate t' of
         Nothing -> return $ NString t'
         Just d  -> return $ NDate d
  parseJSON (Object h) = case fromJSON (Object $ handleMerges h) of
                                Success y -> return $ NMap $ M.toList y
                                _         -> mzero
  parseJSON x@(Array _) = case fromJSON x of
                               Success y -> return $ NList y
                               _         -> mzero
  parseJSON (Bool b) = return $ NString $ show b
  parseJSON (Number y) = return $ NString $ show y
  parseJSON _ = return $ NNil

handleMerges :: H.HashMap T.Text Value -> H.HashMap T.Text Value
handleMerges = H.foldrWithKey go H.empty
  where go k (Object h) m | isMerge k = H.foldrWithKey go m h
        go k v m = H.insert k v m
        isMerge k = k == T.pack "<<"

instance ToJSON Node where
  toJSON (NDate s) = toJSON (NString $ formatTime defaultTimeLocale "%x" s)
  toJSON (NString s) = toJSON s
  toJSON (NMap xs) = toJSON $ M.fromList xs
  toJSON (NList xs) = toJSON xs
  toJSON (NNil) = toJSON ()

data SortDirection = Ascending | Descending deriving (Show, Read, Eq)

data Format = HtmlFormat
            | LaTeXFormat
            | ConTeXtFormat
            | PlainFormat
            | ManFormat
            | RTFFormat
            | TexinfoFormat
            | DocBookFormat
            | OpenDocumentFormat
            deriving (Show, Read, Eq)

instance StringTemplateShows String
  where stringTemplateShow s = s
        stringTemplateFormattedShow "uppercase" s = map toUpper s
        stringTemplateFormattedShow "lowercase" s = map toUpper s
        stringTemplateFormattedShow "capitalize" s = if null s
                                                        then ""
                                                        else toUpper (head s) : tail s
        stringTemplateFormattedShow f _ = error $ "Unknown format: " ++ f

instance ToSElem String
 where toSElem = stShowsToSE

instance ToSElem Node
  where toSElem x = case x of
         NString s   -> toSElem s
         NDate d     -> toSElem d
         NList xs    -> toSElem xs
         NMap xs     -> toSElem $ M.fromList xs
         NNil        -> toSElem ""

parseAsDate :: (ParseTime t) => String -> Maybe t
parseAsDate s =
  msum $ map (\fs -> parsetimeWith fs s) formats
   where parsetimeWith = parseTime defaultTimeLocale
         formats = ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y"]
