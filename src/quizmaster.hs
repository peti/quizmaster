module Main where

import qualified Data.ByteString as BS
import Network.CGI hiding ( Html )
import System.FilePath
import Text.Blaze.Html
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Blaze.Html.Renderer.Utf8

cgiMain :: CGI CGIResult
cgiMain = do
  scriptFilename <- getVarWithDefault "SCRIPT_FILENAME" "."
  Right tmpl <- liftIO $ getDefaultTemplate Nothing "html"
  let markdownReaderOptions :: ReaderOptions
      markdownReaderOptions = def

      htmlWriterOptions :: WriterOptions
      htmlWriterOptions = def
        { writerStandalone = True
        , writerTemplate   = tmpl
        , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        }

      dataDir = takeDirectory scriptFilename </> "../../../data"
      readme  = dataDir </> "README.md"

  setHeader "Content-Type" "text/html; charset=utf-8"
  buf <- liftIO $ UTF8.toString <$> BS.readFile readme
  outputFPS $
    renderHtml $
      writeHtml htmlWriterOptions $
        readMarkdown markdownReaderOptions buf

main :: IO ()
main = runCGI (handleErrors cgiMain)

--------------------------------------------------------------------------------

newtype Ident = Ident String

data Card = Card
  { ident :: Ident
  , question :: Html
  , answer :: Html
  }

data Grade = Fail | Hard | Good | Easy

data Deck = Deck
  { contents :: [Ident]
  , lookup :: Ident -> Card
  , update :: Ident -> Grade -> CGI ()
  , getNext :: CGI Ident
  }
