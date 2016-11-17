{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ViewPatterns, FlexibleContexts #-}
module LojbanHighlighting where

import Protolude
import Hakyll
import Data.Char (isSpace)
import Text.HTML.TagSoup

lookupTranslation :: (StringConv a Text, StringConv Text b) => a -> IO b
lookupTranslation x = do
    tags <- parseTags <$> readFile "jbovlaste.xml"
    return . toS $ findDefinition (toS x) tags
  where
    findDefinition :: Text -> [Tag Text] -> Text
    findDefinition w (TagOpen "valsi" attrs : xs)
      | ("word", w) `elem` attrs = findDefinition' w xs
      | otherwise = findDefinition w xs
    findDefinition w (_ : xs) = findDefinition w xs
    findDefinition w [] = "Keine Definition gefunden"

    findDefinition' :: Text -> [Tag Text] -> Text
    findDefinition' w (TagClose "valsi" : _) = "Keine Definition gefunden"
    findDefinition' w (TagOpen "definition" _ : TagText def : _) = def
    findDefinition' w (_ : xs) = findDefinition' w xs
    findDefinition' w [] = "Keine Definition gefunden"

highlightWord :: (StringConv a [Char], StringConv [Char] b) => a -> IO b
highlightWord (toS -> w) | all isSpace w = return $ toS w
                         | otherwise = toS <$> highlight w
                      where highlight w = (\x -> return ("<span>" ++ w ++ "<span class=\"translation\">" ++ x ++ "</span></span>"))
                                              =<< lookupTranslation w

highlightLojbanBlocks :: (StringConv a [Char], StringConv [Char] b) => a -> IO b
highlightLojbanBlocks (toS -> s) = toS . join <$> parse (tokenize s)
  where
    parse :: [[Char]] -> IO [[Char]]
    parse (x : xs) | x `elem` [ "{jbo}", "{lojban}" ] = highlight xs
                   | otherwise = return . (x :) =<< parse xs
    parse [] = return []

    highlight :: [[Char]] -> IO [[Char]]
    highlight (x : xs) | x `elem` [ "{/jbo}", "{/lojban}" ] = parse xs
                       | otherwise = liftA2 (:) (highlightWord x) (highlight xs)
    highlight [] = return []

    tokenize :: [Char] -> [[Char]]
    tokenize str = let token = ['\n','<','>','\r',' ','\t'] in
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

-- Dieser Compiler lichtet die Syntax von Lojbanblöcken hoch.
-- Ein Lojbanblock beginnt mit {lojban} oder mit {jbo} und endet mit {/lojban} oder {/jbo}.
lojbanCompiler :: Compiler (Item [Char])
lojbanCompiler = getResourceBody >>= withItemBody (unsafeCompiler . highlightLojbanBlocks)

-- Wie `lojbanCompiler`, aber mit Pandoc.
lojbanPandocCompiler :: Compiler (Item [Char])
lojbanPandocCompiler = renderPandoc =<< lojbanCompiler
