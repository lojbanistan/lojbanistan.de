{-# LANGUAGE OverloadedStrings, NoImplicitPrelude,
    ViewPatterns, FlexibleContexts, ScopedTypeVariables #-}
module LojbanHighlighting where

import Protolude
import Hakyll
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Set as S
import Text.HTML.TagSoup

lookupTranslation :: [Tag Text] -> Text -> (S.Set Text, Text)
lookupTranslation tags x = case findDefinition x tags of
    Just res -> (S.empty, res)
    Nothing  -> (S.singleton x, "Keine Definition gefunden")
  where
    findDefinition :: Text -> [Tag Text] -> Maybe Text
    findDefinition w (TagOpen "valsi" attrs : xs)
      | ("word", w) `elem` attrs = findDefinition' w xs
      | otherwise = findDefinition w xs
    findDefinition w (_ : xs) = findDefinition w xs
    findDefinition _ [] = Nothing

    findDefinition' :: Text -> [Tag Text] -> Maybe Text
    findDefinition' _ (TagClose "valsi" : _) = Nothing
    findDefinition' _ (TagOpen "definition" _ : TagText def : _) = Just def
    findDefinition' w (_ : xs) = findDefinition' w xs
    findDefinition' _ [] = Nothing

highlightWord :: [Tag Text] -> Text -> (S.Set Text, Text)
highlightWord tags w
    | T.all isSpace w = (S.empty, w)
    | otherwise = highlight w
        where highlight v = (\x -> "<span>" <> v <> "<span class=\"translation\">" <> x <> "</span></span>")
                                <$> lookupTranslation tags v

highlightLojbanBlocks :: [Tag Text] -> Text -> IO Text
highlightLojbanBlocks tags s = do
    let (set, result) = T.concat <$> parse (asStr tokenize s)
    traverse_ putStrLn $ (\x -> "[Warnung] Keine Definition für " <> x <> " gefunden") <$> S.elems set
    return result
  where
    asStr :: ([Char] -> [[Char]]) -> Text -> [Text]
    asStr f a = toS <$> f (toS a)

    parse :: [Text] -> (S.Set Text, [Text])
    parse (x : xs) | x `elem` [ "{jbo}", "{lojban}" ] = highlight xs
                   | otherwise = (x :) <$> parse xs
    parse [] = return []

    highlight :: [Text] -> (S.Set Text, [Text])
    highlight (x : xs) | x `elem` [ "{/jbo}", "{/lojban}" ] = parse xs
                       | otherwise = let (y,z) = highlight xs in bimap (S.union y) (: z) (highlightWord tags x)
    highlight [] = return []

    -- keine schöne Lösung, hm
    tokenize :: [Char] -> [[Char]]
    tokenize str = let token = ['\n','<','>','\r',' ','\t', '.', ','] in
      case str of
        [] -> []
        [x] -> [[x]]
        (x:y:xs) -> let isToken z = isJust (find (\c -> z == c) token) in
                      case (isToken x, isToken y) of
                        (_, True) -> [[x]] ++ [[y]] ++ tokenize xs
                        (True, False) -> [x] : tokenize (y:xs)
                        (False, False) -> case tokenize (y:xs) of
                                            (a:as) -> (x:a):as
                                            _      -> [[x]]

-- | Dieser Compiler lichtet die Syntax von Lojbanblöcken hoch.
-- Ein Lojbanblock beginnt mit {lojban} oder mit {jbo} und endet mit {/lojban} oder {/jbo}.
lojbanCompiler :: FilePath
               -- ^ Dateiname der Jbovlaste XML
               -> Compiler (Item Text)
lojbanCompiler f = do
  getResourceBody >>= withItemBody (\ib -> unsafeCompiler $ do
    -- unsafeCompiler ok? Vllt. etwas netteres?
    tags <- parseTags <$> readFile f
    highlightLojbanBlocks tags $ toS ib)

-- Wie `lojbanCompiler`, aber mit Pandoc. Der lojbanPandocCompiler sollte immer
-- bevorzugt werden, da jbovlaste im export LaTeX benutzt für die $x_n$ place structures.
lojbanPandocCompiler :: FilePath -> Compiler (Item [Char])
lojbanPandocCompiler f = renderPandoc . (fmap toS) =<< lojbanCompiler f
