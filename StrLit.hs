{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Char as Char
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.Environment as Environment


main :: IO ()
main = do
    args <- Environment.getArgs
    process <- case args of
        ["-toggle-backslash"] -> return toggleBackslash
        ["-add-backslash"] -> return addBackslash
        ["-remove-backslash"] -> return removeBackslash
        ["-toggle-lines"] -> return toggleLines
        ["-add-lines"] -> return addLines
        ["-remove-lines"] -> return removeLines
        _ -> usage
    text <- Text.IO.getContents
    Text.IO.putStr $ Text.unlines $ process $ Text.lines text

usage :: IO a
usage = error $
    "usage: StrLit [ -{add,remove,toggle}-{backslash,lines} ]\n\
    \\n\
    \Convert between plain text and either backslash-continued string\n\
    \literals, or list of lines style strings.  This is to work around\n\
    \haskell's lack of multi-line string literals."

indent :: Text
indent = "    "

toggleBackslash :: [Text] -> [Text]
toggleBackslash lines
    | inferBackslashed lines = removeBackslash lines
    | otherwise = addBackslash lines

inferBackslashed :: [Text] -> Bool
inferBackslashed [] = False
inferBackslashed (line:_) = "\"" `Text.isPrefixOf` Text.stripStart line

addBackslash :: [Text] -> [Text]
addBackslash = map (indent<>) . map3 add1 addn end . map quote . dedent
    where
    nl = "\\n\\"
    add1 s = "\"" <> s <> nl
    addn s = "\\" <> s <> nl
    end s = Just $ "\\" <> s <> "\\n\""

removeBackslash :: [Text] -> [Text]
removeBackslash = map unquote . map3 remove1 removen end
    where
    remove1 = stripPrefix "\"" . spaces . nl
    removen = stripPrefix "\\" . spaces . nl
    end = Just . stripPrefix "\\" . spaces . stripSuffix "\\n\""
    spaces = Text.dropWhile (==' ')
    nl = stripSuffix "\\n\\"

toggleLines :: [Text] -> [Text]
toggleLines lines
    | inferList lines = removeLines lines
    | otherwise = addLines lines

inferList :: [Text] -> Bool
inferList [] = False
inferList (line:_) = "[" `Text.isPrefixOf` Text.stripStart line

addLines :: [Text] -> [Text]
addLines = map3 add1 addn end . map quote . dedent
    where
    add1 line = indent <> "[ \"" <> line <> "\""
    addn line = indent <> ", \"" <> line <> "\""
    end line = Just $ addn line <> "\n" <> indent <> "]"

removeLines :: [Text] -> [Text]
removeLines = map unquote . map3 remove1 removen (const Nothing)
    where
    remove = Text.dropWhile (==' ') . stripSuffix "\""
    remove1 = stripPrefix "[ \"" . remove
    removen = stripPrefix ", \"" . remove

dedent :: [Text] -> [Text]
dedent lines = map (Text.drop indent) lines
    where
    indent = if null lines then 0
        else minimum $ map (Text.length . Text.takeWhile Char.isSpace) lines

quote :: Text -> Text
quote = Text.replace "\"" "\\\""

unquote :: Text -> Text
unquote = Text.replace "\\\"" "\""

map3 :: (a -> b) -> (a -> b) -> (a -> Maybe b) -> [a] -> [b]
map3 _ _ _ [] = []
map3 f g end (x:xs) = f x : go xs
    where
    go [] = []
    go [x] = maybe [] (:[]) (end x)
    go (x:xs) = g x : go xs

stripSuffix :: Text -> Text -> Text
stripSuffix s text =
    maybe (error $ "expected suffix " <> show s <> " on " <> show text) id $
        Text.stripSuffix s text

stripPrefix :: Text -> Text -> Text
stripPrefix s text =
    maybe (error $ "expected prefix " <> show s <> " on " <> show text) id $
        Text.stripPrefix s text
