{-# LANGUAGE OverloadedStrings, NoImplicitPrelude,
    FlexibleContexts, ScopedTypeVariables #-}
module LojbanHighlighting where

import Prelude (String)
import Protolude
import Hakyll
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Set as S
import Text.HTML.TagSoup
import Control.Monad.Writer hiding ((<>))

lookupTranslation :: [Tag Text] -> Text -> Writer (S.Set Text) Text
lookupTranslation tags x = case findDefinition x tags of
    Just res -> return res
    Nothing  -> writer ("Keine Definition gefunden", S.singleton x)
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

highlightWord :: [Tag Text] -> Text -> Writer (S.Set Text) Text
highlightWord tags w
    | T.all isSpace w = return w
    | otherwise = highlight w
        where highlight v = (\x -> "<span>" <> v <> "<span class=\"translation\">" <> x <> "</span></span>")
                                <$> lookupTranslation tags v

highlightLojbanBlocks :: [Tag Text] -> Text -> IO Text
highlightLojbanBlocks tags s = do
    let (result, set) = runWriter (T.concat <$> lojbanize (asStr tokenize s))
    traverse_ putStrLn $ (\x -> "[Warnung] Keine Definition für " <> x <> " gefunden") <$> S.elems set
    return result
  where
    asStr :: (String -> [String]) -> Text -> [Text]
    asStr f a = toS <$> f (toS a)

    lojbanize :: [Text] -> Writer (S.Set Text) [Text]
    lojbanize xs = transformBracket "{solution}" "{/solution}" ["<span class=\"solution\">"] ["</span>"] identity identity
               <$> transformBrackets [("{lojban}","{/lojban}"), ("{jbo}", "{/jbo}")] (highlightWord tags) xs

    transformBrackets :: Monad m => [(Text,Text)] -> (Text -> m Text) -> [Text] -> m [Text]
    transformBrackets ((st,e):xs) f ys = transformBrackets xs f =<< sequence (transformBracket st e [] [] return f ys)
    transformBrackets [] _ xs = return xs

    transformBracket :: Text -- Anfangstext mit dem die Transformation beginnt
                     -> Text -- Endtext mit dem die Transformation endet
                     -> [b]  -- Transformation für den Starttext (meist [])
                     -> [b]  -- Transformation für den Endtext (meist [])
                     -> (Text -> b) -- Transformation für Text der nicht innerhalb der Klammern steht (meist id)
                     -> (Text -> b) -- Transformation für Text der innerhalb der Klammern steht
                     -> [Text] -- Der Text als token liste
                     -> [b] -- Das Resultat der Transformation
    transformBracket start end fs fe id' f xs' = transformBracket' xs' False
      where transformBracket' (x:xs) b | x == start && not b = fs <> transformBracket' xs (not b)
                                       | x == end && b = fe <> transformBracket' xs (not b)
                                       | b = f x : transformBracket' xs b
                                       | otherwise = id' x : transformBracket' xs b
            transformBracket' [] _ = []

    -- keine schöne Lösung, hm
    tokenize :: String -> [String]
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
lojbanCompiler f =
  getResourceBody >>= withItemBody (\ib -> unsafeCompiler $ do
    -- unsafeCompiler ok? Vllt. etwas netteres?
    tags <- parseTags <$> readFile f
    highlightLojbanBlocks tags $ toS ib)

-- Wie `lojbanCompiler`, aber mit Pandoc. Der lojbanPandocCompiler sollte immer
-- bevorzugt werden, da jbovlaste im export LaTeX benutzt für die $x_n$ place structures.
lojbanPandocCompiler :: FilePath -> Compiler (Item String)
lojbanPandocCompiler f = renderPandoc . map toS =<< lojbanCompiler f
