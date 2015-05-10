module Main where

import Control.Monad
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Time.Calendar
-- import Data.Time.Format
import Network.CGI hiding ( Html )
import Prelude hiding ( head )
import System.FilePath
import System.Random
-- import Text.Blaze.Html
-- import Text.Blaze.Html.Renderer.Pretty
-- import Text.Blaze.XHtml1.Strict hiding ( style, map )
-- import Text.Blaze.XHtml1.Strict.Attributes hiding ( title )
-- import qualified Text.Pandoc.UTF8 as UTF8

datadir :: FilePath
datadir = "/home/simons/src/quizmaster/htdocs"

type Ident = FilePath

data MetaData = MetaData
  { hideUntil :: Day
  , related   :: [Ident]
  }
  deriving (Show, Read)

type Manifest = Map Ident MetaData

parseManifestEntry :: [String] -> (Ident, MetaData)
parseManifestEntry [file] = (file, MetaData (toEnum 0) [])
parseManifestEntry (file:md) = (file, read (unwords md))
parseManifestEntry line = error $ "parseManifestEntry: invalid line " ++ show line

loadManifest :: IO Manifest
loadManifest = do
  buf <- readFile $ datadir </> "manifest"
  return $ Map.fromList (map (parseManifestEntry . words) (lines buf))

selectRandomCard :: Manifest -> IO Ident
selectRandomCard cards = do
  j <- randomRIO (0, Map.size cards - 1)
  return $ fst $ Map.elemAt j cards

cgiMain :: CGI CGIResult
cgiMain = do
  scheme <- getVarWithDefault "REQUEST_SCHEME" "http"
  host <- getVarWithDefault "HTTP_HOST" "no.such.name" -- TODO: error handling
  cards <- liftIO loadManifest
  nextCard <- liftIO (selectRandomCard cards)
  let nextUrl = concat [ scheme, "://", host, nextCard ]
  redirect nextUrl
  -- env <- getVars
  -- args <- getInputs
  -- setHeader "Content-Type" "text/html; charset=utf-8"
  -- outputFPS $ UTF8.fromStringLazy $ renderHtml $ docTypeHtml $ do
  --   head $ do meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  --             title "Quizmaster CGI Environment"
  --   body $ do h1 "Known Cards:"
  --             table ! cellpadding "2" $
  --               forM_ (Map.toAscList cards) $ \(key,val) ->
  --                 tr $ td (toHtml key) >> td (toHtml (show val))
  --             h1 "CGI Arguments:"
  --             table ! cellpadding "2" $
  --               forM_ args $ \(key,val) ->
  --                 tr $ td (toHtml key) >> td (toHtml val)
  --             h1 "CGI Environment:"
  --             table ! cellpadding "2" $
  --               forM_ env $ \(key,val) ->
  --                 tr $ td (toHtml key) >> td (toHtml val)

main :: IO ()
main = runCGI (handleErrors cgiMain)
