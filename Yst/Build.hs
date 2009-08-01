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

module Yst.Build (buildSite)
where
import Yst.Types
import Yst.Util
import Yst.Render
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List
import System.FilePath
import System.Directory
import System.Exit
import System.Time (ClockTime(..))
import System.IO.UTF8
import System.IO (stderr)
import Prelude hiding (readFile, putStrLn, print, writeFile)
import Control.Monad

dependencies :: Site -> String -> [FilePath]
dependencies site url =
  let page = case M.lookup url (pageIndex site) of
                  Nothing   -> error $ "Tried to get dependencies for nonexistent page: " ++ url
                  Just pg   -> pg
      layout = sourceDir site </> stripStExt (fromMaybe (defaultLayout site) $ layoutFile page) <.> "st"
      requires = map (sourceDir site </>) $ requiresFiles page
      srcdir = sourceDir site </>
                 case sourceFile page of
                       TemplateFile f -> stripStExt f <.> "st"
                       SourceFile f   -> f
      dataFiles = map (\(_,(f,_)) -> dataDir site </> f) $ pageData page
  in  indexFile site : layout : srcdir : (requires ++ dataFiles)

buildSite :: Site -> IO ()
buildSite site = do
  files <- liftM (filter (/=".") . map (makeRelative $ filesDir site)) $ getDirectoryContentsRecursive $ filesDir site
  let pages = M.keys $ pageIndex site
  let overlap = files `intersect` pages
  unless (null overlap) $ forM_ overlap
    (\f -> hPutStrLn stderr $ "Warning: the page '" ++ f ++ "' will overwrite the file by the same name.")
  forM_ files $ \file ->
     updateFile site file
  forM_ pages $ \page ->
     case M.lookup page (pageIndex site) of
          Just  pg -> updatePage site pg
          Nothing  -> error $ "Couldn't find page " ++ page

updateFile :: Site -> FilePath -> IO ()
updateFile site file = do
  let destpath = deployDir site </> file
  let srcpath = filesDir site </> file
  srcmod <- getModificationTime srcpath
  destmod <- catch (getModificationTime destpath) (\_ -> return $ TOD 0 0)
  if srcmod > destmod
     then do
       createDirectoryIfMissing True $ takeDirectory destpath
       hPutStrLn stderr $ "Updating " ++ destpath
       copyFile srcpath destpath
     else return ()

updatePage :: Site -> Page -> IO ()
updatePage site page = do
  let destpath = deployDir site </> pageUrl page
  let deps = dependencies site $ pageUrl page
  forM_ deps $ \dep -> do
    exists <- doesFileExist dep
    unless exists $ do
      hPutStrLn stderr $ "Missing dependency: " ++ dep
      hPutStrLn stderr $ "Aborting!  Cannot build " ++ destpath
      exitWith $ ExitFailure 3
  depsmod <- mapM getModificationTime deps
  destmod <- catch (getModificationTime destpath) (\_ -> return $ TOD 0 0)
  if maximum depsmod > destmod
     then do
       createDirectoryIfMissing True $ takeDirectory destpath
       hPutStrLn stderr $ "Updating " ++ destpath
       renderPage site page >>= writeFile destpath
     else return ()
