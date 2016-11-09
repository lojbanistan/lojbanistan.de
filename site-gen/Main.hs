{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Main where

import Protolude
import qualified System.FilePath.Posix as FP

import Hakyll

main :: IO ()
main = hakyll $ do
  -- alles aus /static nach /
  match "static/**" $ do
    route $ customRoute
      $ FP.joinPath . tailSafe . FP.splitPath . toFilePath
    compile copyFileCompiler

  -- Bilder bleiben, wo sie sind
  match "img/*" $ do
    route idRoute
    compile copyFileCompiler

  -- kompiliere Artikel
  -- <id>s sind die Dateinamen artikel/das-ist-eine-id.md
  -- Das Layout ist /artikel/<id>.html
  match "artikel/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler >>= relativizeUrls

  -- Liste an Artikeln
  match "templates/index.html" $ do
    route $ constRoute "index.html"
    compile $ do
      let ctx = listField "artikel" defaultContext (loadAll "artikel/*")
      getResourceString >>= applyAsTemplate ctx

