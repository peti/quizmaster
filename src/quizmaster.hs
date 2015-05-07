{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding ( head )
import qualified Data.ByteString as BS
import Network.CGI hiding ( Html )
import System.FilePath
import Control.Monad
import Text.Blaze.Html
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.XHtml1.Strict hiding ( style )
import Text.Blaze.XHtml1.Strict.Attributes hiding ( title )

cgiMain :: CGI CGIResult
cgiMain = do
  env <- getVars
  setHeader "Content-Type" "text/html; charset=utf-8"
  args <- getInputs
  outputFPS $ UTF8.fromStringLazy $ renderHtml $ docTypeHtml $ do
    head $ do meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
              title "Quizmaster CGI Environment"
    body $ do h1 "Quizmaster CGI Arguments:"
              table ! cellpadding "2" $
                forM_ args $ \(key,val) ->
                  tr $ td (toHtml key) >> td (toHtml val)
              h1 "Quizmaster CGI Environment:"
              table ! cellpadding "2" $
                forM_ env $ \(key,val) ->
                  tr $ td (toHtml key) >> td (toHtml val)

main :: IO ()
main = runCGI (handleErrors cgiMain)
