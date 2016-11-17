{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Main where

import Data.Char (isSpace)
import Data.List (words, unwords, lines, unlines, init, groupBy)
-- Wegen Protolude#17 (https://github.com/sdiehl/protolude/issues/17) müssen wir Data.Monoid.(<>) importieren
import Data.Monoid ((<>))
import Protolude hiding ((<>))
import qualified Prelude as P
import qualified System.FilePath.Posix as FP

import Hakyll
import LojbanHighlighting

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
    let ctx = articleDependenciesContext <> defaultContext
    compile $ lojbanPandocCompiler
          >>= loadAndApplyTemplate (fromFilePath "templates/default.html") ctx
          >>= relativizeUrls

  -- Liste an Artikeln
  match "templates/index.html" $ do
    route $ constRoute "index.html"
    compile $ do
      let ctx = listField "artikel" defaultContext (loadAll "artikel/*") <> defaultContext 
      getResourceString
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate (fromFilePath "templates/default.html") ctx
        >>= relativizeUrls

  match "templates/*" $
    compile templateCompiler

-- Ein Kontext der das `aufbauendAuf` Feld eines Artikels ausliest
-- und danach alle Metadaten zu diesen gelisteten Artikeln ausliest
-- und in der aufbauendAuf template Variable speichert.
--
-- Das aufbauendAuf Feld im Artikel kann in einer YAML-artigen Syntax definiert werden.
-- TODO: Erlaube mehr Extensions als .md
articleDependenciesContext :: Context a
articleDependenciesContext = listField "aufbauendAuf" defaultContext $ do
  identifier <- getUnderlying
  metadata <- getMetadata identifier
  case lookupStringList "aufbauendAuf" metadata of
    Nothing -> return []
    Just xs -> traverse load $ map (\x -> fromFilePath ("artikel/" ++ x ++ ".md")) xs
