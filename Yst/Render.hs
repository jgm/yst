{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}
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

module Yst.Render (renderPage)
where
import Yst.Types
import Yst.Util
import Yst.Data
import System.Directory
import Text.Pandoc hiding (Format)
import Lucid
import Data.Char
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Text.StringTemplate
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.FilePath
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
#else
import Prelude hiding (readFile, putStrLn, print, writeFile)
import System.IO.UTF8
#endif
import Data.Time
import Control.Monad
import Text.Pandoc.Error (handleError)
import Data.Monoid

-- | @relUrl a b@ returns a URL for @b@ relative to @a@.  So, for
-- example, @relUrl "a" "a/b.html" = "b.html"@,
-- @relUrl "" "a/b.html" = "a/b.html"@, and @relUrl "a" "b.html" = "../b.html"@
relUrl :: String -> String -> String
relUrl relto url = intercalate "/" $ relPath ++ [urlBase]
  where relPath = relPaths reltoPaths urlPaths
        (reltoPaths, urlPaths) = dropCommon (wordsBy (=='/') relto) (wordsBy (=='/') urlDir)
        urlBase = takeUrlBase url
        urlDir = takeUrlDir url

takeUrlBase :: String -> String
takeUrlBase = reverse . takeWhile (/= '/') . reverse

takeUrlDir :: String -> String
takeUrlDir = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse

relPaths :: [String] -> [String] -> [String]
relPaths [] ys = ys
relPaths (_:xs) ys  = ".." : relPaths xs ys

dropCommon :: (Eq a) => [a] -> [a] -> ([a],[a])
dropCommon (x:xs) (y:ys) | x == y = dropCommon xs ys
dropCommon xs ys = (xs,ys)

renderNav :: NavStyle -> String -> [NavNode] -> String
renderNav sty targeturl nodes = TL.unpack $ renderText $
  ul_ [class_ classes] $ mapM_ (renderNavNode sty targeturl) nodes
  where classes = case sty of
                       TopNav -> "nav navbar-nav"
                       SideNav -> "nav nav-pills nav-stacked"

renderNavNode :: NavStyle -> String -> NavNode -> Html ()
renderNavNode _ targeturl (NavPage tit pageurl) =
  li_ [class_ "active" | pageurl == targeturl]
   (a_ [href_ pageurl'] (toHtml tit))
    where targetdir = takeUrlDir targeturl
          pageurl' = T.pack $ relUrl targetdir pageurl
renderNavNode TopNav targeturl (NavMenu tit nodes) = li_ [] $
    do a_ [class_ "dropdown-toggle", data_ "toggle" "dropdown"] (toHtml tit)
       ul_ [class_ "dropdown-menu"]
         (mapM_ (renderNavNode TopNav targeturl) nodes)
renderNavNode SideNav targeturl node@(NavMenu tit nodes) = li_ [] $
    do a_ [id_ buttonId,
           class_ "submenu collapsed", data_ "toggle" "collapse",
           data_ "target" ("#" <> submenuId)]
           (toHtml tit)
       ul_ [id_ submenuId,
            class_ ("nav collapse" <> if expanded then " in" else "")]
           (mapM_ (renderNavNode SideNav targeturl) nodes)
    where submenuId = T.replace " " "_" $ T.pack ("menu-" ++ tit)
          buttonId = T.replace " " "_" $ T.pack ("menu-label-" ++ tit)
          expanded = targeturl `elem` (getUrls node)
          getUrls (NavPage _ u) = [u]
          getUrls (NavMenu _ ns) = concatMap getUrls ns

formatFromExtension :: FilePath -> Format
formatFromExtension f = case (map toLower $ takeExtension f) of
                             ".html"  -> HtmlFormat
                             ".xhtml" -> HtmlFormat
                             ".latex" -> LaTeXFormat
                             ".tex"   -> LaTeXFormat
                             ".context" -> ConTeXtFormat
                             ".1"     -> ManFormat
                             ".rtf"   -> RTFFormat
                             ".texi"  -> TexinfoFormat
                             ".db"    -> DocBookFormat
                             ".fodt"  -> OpenDocumentFormat
                             ".txt"   -> PlainFormat
                             ".markdown" -> PlainFormat
                             _       -> HtmlFormat

renderPage :: Site -> Page -> IO String
renderPage site page = do
  let menuHtml = renderNav (navstyle site) (pageUrl page) (navigation site)
  let layout = fromMaybe (defaultLayout site) $ layoutFile page
  srcDirs <- mapM canonicalizePath $ sourceDir site
  gs <- mapM directoryGroupRecursive srcDirs
  let g = foldl1 mergeSTGroups gs
  attrs <- forM (pageData page) $ \(k, v) -> getData site v >>= \n -> return (k,n)
  todaysDate <- liftM utctDay Data.Time.getCurrentTime
  let root' = case length (filter (=='/') $ pageUrl page) of
                    0  -> []
                    n  -> concat $ replicate n ("../" :: String)
  rawContents <-
    case sourceFile page of
          SourceFile sf   -> liftM (filter (/='\r')) $ searchPath srcDirs sf >>= readFile
          TemplateFile tf -> do
            templ <- getTemplate tf g
            return $ render
                    . setManyAttrib attrs
                    . setAttribute "root" root'
                    . setAttribute "gendate" todaysDate
                    $ templ
  layoutTempl <- getTemplate layout g
  let format = formatFromExtension (stripStExt layout)
  contents <- converterForFormat format (T.pack rawContents)
  return $ render
         . setManyAttrib attrs
         . setAttribute "sitetitle" (siteTitle site)
         . setAttribute "pagetitle" (pageTitle page)
         . setAttribute "gendate" todaysDate
         . setAttribute "contents" (T.unpack contents)
         . setAttribute "root" root'
         . setAttribute "nav" menuHtml
         $ layoutTempl

converterForFormat :: Format -> T.Text -> IO T.Text
converterForFormat PlainFormat s = return s
converterForFormat f s = handleError =<< runIO (reader s >>= writer)
  where wopts name = def{ writerExtensions = getDefaultExtensions name }
        reader = readMarkdown def{ readerExtensions =
                           getDefaultExtensions "markdown" }
        writer = case f of
                   HtmlFormat          -> writeHtml5String (wopts "html5")
                   LaTeXFormat         -> writeLaTeX (wopts "latex")
                   ConTeXtFormat       -> writeConTeXt (wopts "context")
                   ManFormat           -> writeMan (wopts "man")
                   RTFFormat           -> writeRTF (wopts "rtf")
                   DocBookFormat       -> writeDocbook5 (wopts "docbook5")
                   TexinfoFormat       -> writeTexinfo (wopts "texinfo")
                   OpenDocumentFormat  -> writeOpenDocument (wopts "opendocument")
                   PlainFormat         -> error "should not happen"

getTemplate :: Stringable a => String -> STGroup a -> IO (StringTemplate a)
getTemplate templateName templateGroup = do
  let template = case getStringTemplate (stripStExt templateName) templateGroup of
                       Just pt  -> pt
                       Nothing  -> error $ "Could not load template: " ++ templateName
  case checkTemplate template of
       (Just parseErrors, _, _ )       -> errorExit 17 $ "Error in template '" ++ templateName ++
                                             "': " ++ parseErrors
       (_, _, Just templatesNotFound)  -> errorExit 21 $ "Templates referenced in template '" ++ templateName ++
                                             "' not found: " ++ (intercalate ", " templatesNotFound)
       (_, _, _)                       -> return ()
  return template
