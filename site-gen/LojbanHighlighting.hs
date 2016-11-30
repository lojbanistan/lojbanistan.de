{-# LANGUAGE OverloadedStrings, NoImplicitPrelude,
    ViewPatterns, FlexibleContexts, ScopedTypeVariables #-}
module LojbanHighlighting where

import Protolude
import Hakyll
import Data.Char (isSpace)
import qualified Data.Text as T
import Text.HTML.TagSoup

-- TODO get out of IO
lookupTranslation :: [Tag Text] -> Text -> IO Text
lookupTranslation tags x = do
    case findDefinition x tags of
      Just res -> return res
      Nothing  -> do
        putStrLn ("[Warnung] Keine Definition für " <> x <> " gefunden")
        return "Keine Definition gefunden"
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

-- TODO get out of IO, move the tags somewhere else
highlightWord :: [Tag Text] -> Text -> IO Text
highlightWord tags w
    | T.all isSpace w = return $ w
    | otherwise = highlight w
        where highlight v = (\x -> return ("<span>" <> v <> "<span class=\"translation\">" <> x <> "</span></span>"))
                                =<< lookupTranslation tags v

-- TODO get out of IO, move the tags somewhere else
highlightLojbanBlocks :: [Tag Text] -> Text -> IO Text
highlightLojbanBlocks tags s =
  -- Warum T.concat und parse :: [Text] -> [Text], wenn man es eh zusammenschmeißt?
  T.concat <$> parse (asStr tokenize s)
  where
    asStr :: ([Char] -> [[Char]]) -> Text -> [Text]
    asStr f a = toS <$> f (toS a)
    parse :: [Text] -> IO [Text]
    parse (x : xs) | x `elem` [ "{jbo}", "{lojban}" ] = highlight xs
                   | otherwise = return . (x :) =<< parse xs
    parse [] = return []

    highlight :: [Text] -> IO [Text]
    highlight (x : xs) | x `elem` [ "{/jbo}", "{/lojban}" ] = parse xs
                       | otherwise = liftA2 (:) (highlightWord tags x) (highlight xs)
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

-- Wie `lojbanCompiler`, aber mit Pandoc.
lojbanPandocCompiler :: FilePath -> Compiler (Item [Char])
lojbanPandocCompiler f = renderPandoc . (fmap toS) =<< lojbanCompiler f
