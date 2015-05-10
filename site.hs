{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding ( head, span )
import Data.Monoid
import Data.Char
import Data.String
import Data.List hiding ( head, span )
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.CGI hiding ( Html )
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.XHtml1.Strict hiding ( div, style )
import qualified Text.Blaze.XHtml1.Strict as HTML
import Text.Blaze.XHtml1.Strict.Attributes hiding ( title, span )
import Text.Pandoc hiding ( Div )
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Printf
import System.FilePath

data Op = Plus | Minus | Mul | Div
  deriving (Bounded, Enum, Show)

makeCard :: Op -> Integer -> Integer -> Maybe (String, String)
makeCard Plus x y  | x >= y             = Just (printf "%d + %d = ?" x y, printf "%d" (x + y))
                   | otherwise          = Nothing
makeCard Minus x y                      = Just (printf "%d - %d = ?" x y, printf "%d" (x - y))
makeCard Mul x y   | x >= y             = Just (printf "%d × %d = ?" x y, printf "%d" (x * y))
                   | otherwise          = Nothing
makeCard Div x y   | x < y              = Nothing
                   | x `mod` y == 0     = Just (printf "%d ÷ %d = ?" x y, printf "%d" (x `div` y))
                   | otherwise          = Just (printf "%d ÷ %d = ?" x y, printf "%d, Rest %d" (x `div` y) (x `mod` y))

pCenter :: Html -> Html
pCenter = p ! style "text-align:center;"

pRight :: Html -> Html
pRight = p ! style "text-align:right;"

myBody :: Html -> Html
myBody = body ! style "margin-left:5%; margin-right:5%;"

main :: IO ()
main = do
  cards <- forM [minBound..maxBound] $ \op ->
    forM [1..10] $ \x ->
      forM [1..10] $ \y ->
        case makeCard op x y of
          Nothing -> return []
          Just (question, answer) -> do
            let pageId = toLower <$> intercalate "-" [show op, show x, show y]
                qfile  = pageId ++ ".html"
                afile  = pageId ++ "-answer.html"

                gradeLink :: String -> Html
                gradeLink grade = a ! href (toValue ("/cgi-bin/quizmaster?card=/" ++ qfile ++ "&grade=" ++ fmap toLower grade))
                                    ! style "border:outset; padding:5px; text-decoration:none;"
                                    $ toHtml grade

            writeFile ("htdocs" </> qfile) $ renderHtml $ docTypeHtml $ do
              head $ do meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
                        title "Mental Arithmetic"
              myBody $ do pRight $ a ! href (toValue afile) ! style "border:outset; padding:5px; text-decoration:none;" $ "Show answer"
                          pCenter $ toHtml question
            writeFile ("htdocs" </> afile) $ renderHtml $ docTypeHtml $ do
              head $ do meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
                        title "Mental Arithmetic"
              myBody $ do pRight $ do
                            gradeLink "Again"
                            preEscapedToHtml ("&nbsp;"::String)
                            gradeLink "Hard"
                            preEscapedToHtml ("&nbsp;"::String)
                            gradeLink "Good"
                            preEscapedToHtml ("&nbsp;"::String)
                            gradeLink "Easy"
                          pCenter $ toHtml question
                          hr
                          pCenter $ toHtml answer
            return ['/':qfile]
  writeFile "htdocs/manifest" (unlines $ concat $ concat $ concat cards)
