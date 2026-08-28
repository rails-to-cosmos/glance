{-# LANGUAGE TemplateHaskell #-}

-- | The document every served page wears; styles inline, the renderer its one asset.
module Glance.Web.Page.Style ( page
                             , fontAssets
                             , fontFace
                             ) where

import Glance.Web.Page.Popups (chromeBoxes, chromeFeet, chromeHeads, boxes, veiled, washed)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import System.FilePath (takeExtension)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Glance.Web.Base (escape, logLinesDefault)
import Glance.Web.Theme (themeCSS, themeIds, themeOverrides)


-- | @#a.on,#b.on,…@ — the same list, each wearing the class that shows it.
onEach :: Text -> Text
onEach = T.intercalate "," . map (<> ".on") . T.splitOn ","

-- | @html.stale #a,…@ — the wash names each surface under the stale root.
staleEach :: Text -> Text
staleEach = T.intercalate "," . map ("html.stale " <>) . T.splitOn ","

-- | @assets\/page.css@ as it ships, embedded at COMPILE time like the renderer.
rawPageCss :: ByteString
rawPageCss = $(makeRelativeToProject "assets/page.css" >>= embedFile)

-- | The stylesheet ready to splice: @page.css@ with its @\/* … *\/@ comments and
--   blank lines dropped (they carry the rules' law, not a byte of the served
--   page), and every COMPILE-TIME selector spliced from 'Glance.Web.Page.Popups'
--   so this file cannot fork from that registry.  The one @{{THEME}}@ placeholder
--   is the sole per-request seam, filled in 'page'; the geometry vars and
--   @--glance-mono@ stand literally in the file.
styleBody :: Text
styleBody = foldr (\(tok, val) -> T.replace tok val) trimmed tokens
  where
    trimmed = T.intercalate "\n"
            . filter (\l -> not (T.null (T.strip l)) && not ("/*" `T.isInfixOf` l))
            . T.lines
            $ TE.decodeUtf8 rawPageCss
    tokens =
      [ ("{{LOGN}}",         T.pack (show logLinesDefault))
      , ("{{VEILED}}",       veiled)
      , ("{{VEILED_ON}}",    onEach veiled)
      , ("{{BOXES}}",        boxes)
      , ("{{CHROME_BOXES}}", chromeBoxes)
      , ("{{CHROME_HEADS}}", chromeHeads)
      , ("{{CHROME_FEET}}",  chromeFeet)
      , ("{{WASHED}}",       washed)
      , ("{{WASHED_STALE}}", staleEach washed)
      ]

fontAssets :: [FilePath]
fontAssets = ["JetBrainsMono-Regular.woff2", "JetBrainsMono-Regular.ttf"]

fontFace :: Maybe FilePath -> Text
fontFace Nothing     = ""
fontFace (Just name) = T.concat
  [ "  @font-face{font-family:\"JetBrains Mono\";font-display:swap;"
  , "src:url(\"", T.pack name, "\") format(\"", format, "\")}" ]
  where format | takeExtension name == ".woff2" = "woff2"
               | otherwise                      = "truetype"

-- | BODY wrapped in a document titled TITLE, with HEAD opening the style block.
page :: Text -> [(Text, [(Text, Text)])] -> Text -> Text -> Text
page head' colours title body = T.unlines
  [ "<!doctype html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>" <> escape title <> "</title>"
  , "<style>" <> (if T.null head' then "" else "\n" <> head')
  -- The CSS body is 'assets/page.css'; only @{{THEME}}@ is per-request.  The
  -- geometry vars, @--glance-mono@ and every law comment live in that file.
  , T.replace "{{THEME}}" (T.stripEnd (themeCSS <> themeOverrides colours)) styleBody
  , "</style>"
  -- One line, so the suite's glue extractor still finds the one inline script.
  , "<script>" <> themeBoot <> "</script>"
  , "</head>"
  , "<body>"
  , body <> "</body>"
  , "</html>"
  ]

-- | The head script: the remembered LOOK pinned before the first paint -- the
--   theme.  A value the page does not know is ignored, so the default look
--   survives a hand-edited store.
themeBoot :: Text
themeBoot = T.concat
  [ "try{", stamp "theme" "glance-theme" themeIds, "}catch(e){}" ]
  where
    stamp prop key ids = T.concat
      [ "var v=localStorage.getItem(\"", key, "\");"
      , "if(", T.intercalate "||" [ "v===\"" <> name <> "\"" | name <- ids ]
      , ")document.documentElement.dataset.", prop, "=v;" ]
