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
import Data.Time
import Text.StringTemplate
import qualified Data.Map as M

data Site = Site {
    siteTitle     :: String
  , sourceDir     :: FilePath
  , dataDir       :: FilePath
  , filesDir      :: FilePath
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
