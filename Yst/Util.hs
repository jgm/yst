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

module Yst.Util (stripBlanks, parseAsDate, stripStExt, getStrAttrMaybe, getStrAttrWithDefault, getStrListWithDefault, fromNString, getDirectoryContentsRecursive, searchPath, errorExit)
where
import Yst.Types
import System.Exit
import System.FilePath
import System.IO (stderr)
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
import System.IO (hPutStrLn)
#else
import System.IO.UTF8 (hPutStrLn)
#endif
import System.Directory
import Control.Monad
import Data.Time
import Data.List (intercalate)
import Data.Char (isSpace)
import System.Locale (defaultTimeLocale)

-- | Strip blank lines from a file.
stripBlanks :: String -> String
stripBlanks = intercalate "\n" . filter (not . all isSpace) . lines

parseAsDate :: (ParseTime t) => String -> Maybe t
parseAsDate s =
  msum $ map (\fs -> parsetimeWith fs s) formats
   where parsetimeWith = parseTime defaultTimeLocale
         formats = ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y"]

stripStExt :: FilePath -> FilePath
stripStExt f =
  if (takeExtension f == ".st")
     then dropExtension f
     else f

getStrAttrMaybe :: String -> [(String, Node)] -> Maybe String
getStrAttrMaybe attr xs =
  case lookup attr xs of
    Just (NString s) -> Just s
    Just _ -> error $ attr ++ " must have string value."
    Nothing -> Nothing

getStrAttrWithDefault :: String -> String -> [(String, Node)] -> String
getStrAttrWithDefault attr def xs =
  maybe def id $ getStrAttrMaybe attr xs

getStrListWithDefault :: String -> String -> [(String, Node)] -> [String]
getStrListWithDefault attr def xs =
  case lookup attr xs of
    Just (NString s) -> [s]
    Just (NList ys) -> map nodeToString ys
    Just _ -> formatError
    Nothing -> [def]
  where nodeToString (NString s) = s
        nodeToString _ = formatError
        formatError = error $ attr ++ " must be a string or list of strings."

fromNString :: Node -> String
fromNString (NString s) = s
fromNString x = error $ "Expected string value, got " ++ show x

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive path = do
  isDir <- doesDirectoryExist path
  if isDir
     then do
       contents <- getDirectoryContents path
       let contents' = map (path </>) $ filter (`notElem` ["..","."]) contents
       children <- mapM getDirectoryContentsRecursive contents'
       return (concat children)
     else return [path]

searchPath :: [FilePath] -> FilePath -> IO FilePath
searchPath [] file = return file       -- may or may not exist, but we tried.
searchPath (dir:dirs) file = do
  exists <- doesFileExist curFile
  if exists then return curFile else searchPath dirs file
    where curFile = dir </> file

errorExit :: Int -> String -> IO ()
errorExit lvl msg = hPutStrLn stderr msg >> exitWith (ExitFailure lvl)
