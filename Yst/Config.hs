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

module Yst.Config (parseConfigFile, parseIndexFile) where
import System.FilePath
import Yst.Types
import Yst.Util
import Yst.Yaml
import Yst.Data
import Data.Char
import qualified Data.Map as M

parseConfigFile :: FilePath -> IO Site
parseConfigFile configfile = do
  node <- readYamlFile configfile
  case node of
       NMap xs -> do
         let indexfile = getStrAttrWithDefault "indexfile" "index.yaml" xs
         (ind, nav) <- parseIndexFile indexfile
         return Site{ siteTitle     = getStrAttrWithDefault "title" "" xs
                    , sourceDir     = getStrAttrWithDefault "sourcedir" "." xs
                    , dataDir       = getStrAttrWithDefault "datadir" "." xs
                    , filesDir      = getStrAttrWithDefault "filesdir" "files" xs
                    , deployDir     = getStrAttrWithDefault "deploydir" "site" xs
                    , defaultLayout = getStrAttrWithDefault "layout" "layout" xs
                    , indexFile     = indexfile
                    , pageIndex     = M.fromList $ map (\pg -> (pageUrl pg, pg)) ind
                    , navigation    = nav
                    }
       _       -> errorExit 7 "Configuration file must be a YAML hash." >> return undefined

parseIndexFile :: FilePath -> IO ([Page], [NavNode])
parseIndexFile indexfile = do
  node <- readYamlFile indexfile
  case node of
        NList xs -> return $ foldr processItem ([], []) xs
        _        -> errorExit 9 "Index file must be a YAML list." >> return undefined

processItem :: Node -> ([Page], [NavNode]) -> ([Page], [NavNode])
processItem (NMap [(s, NList xs)]) (index, nav) = -- a submenu
  (pages ++ index, (NavMenu s navnodes) : nav)
    where (pages, navnodes) = foldr processItem ([], []) xs
processItem (NMap xs) (index, nav) =  -- a page
  (page : index, newnav)
    where page = processPage xs
          newnav = if pageInMenu page
                      then NavPage (pageTitle page) (pageUrl page) : nav
                      else nav
processItem _ _ = error "processItem encountered a non-NMap node."

processPage :: [(String, Node)] -> Page
processPage xs =
  Page { pageData     = case lookup "data" xs of
                             Nothing        -> []
                             Just (NMap ds) -> map (\(k,v) -> (k, parseDataField v)) ds
                             Just _         -> error "data must be a YAML map"
       , layoutFile   = fmap fromNString $ lookup "layout" xs
       , sourceFile   = case (lookup "source" xs, lookup "template" xs) of
                             (Nothing, Nothing)  -> error "No 'source' or 'template' found for page."
                             (Nothing, Just f)   -> TemplateFile $ fromNString f
                             (Just f, Nothing)   -> SourceFile $ fromNString f
                             (Just _, Just _)    -> error "Both 'source' and 'template' specified for the same page."
       , requiresFiles = case lookup "requires" xs of
                              Nothing          -> []
                              Just (NList fs)  -> map getStrAttr fs
                              Just (NString f) -> [f]
                              Just _           -> error "'requires' must be scalar or list"
       , pageUrl      = url
       , pageTitle    = getStrAttrWithDefault "title" (dropExtension url) xs
       , pageInMenu   = (map toLower $ getStrAttrWithDefault "inmenu" "yes" xs) `notElem`
                          ["no","false"]
       }
    where getPageField f = case (fmap fromNString $ lookup f xs) of
                             Just s  -> s
                             Nothing -> error $ "Missing required " ++ f ++ " field in page definition"
          url = getPageField "url"
          getStrAttr (NString s) = s
          getStrAttr x           = error $ "expected string, got " ++ show x


