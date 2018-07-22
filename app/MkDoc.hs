{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy.IO           as TL
import           Lucid
import qualified Text.MMark                  as MMark
import           Text.MMark.Extension        (Block (..), Extension)
import qualified Text.MMark.Extension        as Ext
import qualified Text.MMark.Extension.Common as Ext

main :: IO ()
main = forM_ ["ch02", "ch03", "ch04", "ch05", "ch06", "ch07"] $ \input -> do
  txt <- T.readFile $ mconcat ["note/", input, ".md"]
  case MMark.parse input txt of
    Left errs -> putStrLn (MMark.parseErrorsPretty txt errs)
    Right r ->
      let toc = MMark.runScanner r (Ext.tocScanner (> 1))
      in  TL.writeFile (mkPath input)
          . renderText
          . wrapper
          . MMark.render
          . MMark.useExtensions
            [ Ext.toc "toc" toc
            , Ext.punctuationPrettifier
            , Ext.ghcSyntaxHighlighter
            , Ext.skylighting
            , Ext.mathJax (Just '$')
            , mathJaxBlock
            ]
          $ r
  where
    mkPath path = mconcat ["_site/", path, ".html"]

wrapper :: Html () -> Html ()
wrapper content = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8"]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
      title_ [] "TAPL"
      link_  [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"]
      link_  [rel_ "stylesheet", href_ "../css/hilight.css"]
      link_  [rel_ "stylesheet", href_ "../css/base.css"]
      script_ [ defer_ T.empty , src_ "https://use.fontawesome.com/releases/v5.1.0/js/all.js" ] T.empty
      script_ [ async_ T.empty, src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-AMS_CHTML" ] T.empty
    body_ $
      section_ [class_ "section"] $
        div_ [class_ "container"] $
          div_ [class_ "content"]
            content

mathJaxBlock :: Extension
mathJaxBlock = Ext.blockRender $ \old block ->
  case block of
    b@(CodeBlock mlabel txt) ->
      if mlabel == Just "mathjaxBlock"
        then do
               p_ . forM_ (T.lines txt) $ \x -> toHtml x
               "\n"
        else old b
    other -> old other
