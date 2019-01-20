{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import           RIO
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as TL
import           Lucid
import qualified Text.MMark                  as MMark
import           Text.MMark.Extension        (Block (..), Extension)
import qualified Text.MMark.Extension        as Ext
import qualified Text.MMark.Extension.Common as Ext
import           Text.Megaparsec.Error

main :: IO ()
main = runSimpleApp app

app :: RIO SimpleApp ()
app = forM_ ["ch02", "ch03", "ch04", "ch05", "ch06", "ch07", "ch08"] $ \input -> do
  txt <- readFileUtf8 $ mconcat ["note/", input, ".md"]
  case MMark.parse input txt of
    Left errs -> logError $ displayShow $ errorBundlePretty errs
    Right r ->
      let toc = MMark.runScanner r (Ext.tocScanner (> 1))
      in  writeFileUtf8 (mkPath input)
          . TL.toStrict
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
      link_  [rel_ "stylesheet", href_ "css/hilight.css"]
      link_  [rel_ "stylesheet", href_ "css/base.css"]
      script_ [ defer_ Text.empty , src_ "https://use.fontawesome.com/releases/v5.1.0/js/all.js" ] TL.empty
      script_ [ async_ Text.empty, src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-AMS_CHTML" ] TL.empty
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
               p_ . forM_ (Text.lines txt) $ \x -> toHtml x
               "\n"
        else old b
    other -> old other
