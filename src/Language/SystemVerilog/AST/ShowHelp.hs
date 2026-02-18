{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - Helpers for printing AST items
 -}

module Language.SystemVerilog.AST.ShowHelp
    ( showPad
    , showPadBefore
    , indent
    , unlines'
    , commas
    , indentedParenList
    , showEither
    , showBlock
    ) where

import Data.List (intercalate)

-- add a space to the right of 'show x'.
showPad :: Show t => t -> String
showPad x =
    if null str
        then ""
        else str ++ " "
    where str = show x

-- add a space to the left of 'show x'.
showPadBefore :: Show t => t -> String
showPadBefore x =
    if str == ""
        then ""
        else ' ' : str
    where str = show x

-- if the next character is '\n', print it followed by a tab.
indent :: String -> String
indent s = intercalate "\n" $ map ('\t':) $ filter (not . null) $ lines s

-- split lines with a '\n'.
-- ignore empty lines.
-- delete leading spaces.

unlines' :: [String] -> String
unlines' [] = ""
unlines' xs = intercalate "\n" $ map (dropWhile isSpace) $ filter (not . all isSpace) xs
  where isSpace c = c `elem` [' ', '\t', '\n', '\r']
  
-- merge an array of strings with ',  ' between each item.
commas :: [String] -> String
commas = intercalate ", "

-- prints the parametes, each one indented on a new line.
indentedParenList :: [String] -> String
indentedParenList [] = "()"
indentedParenList [x] = '(' : x ++ ")"
indentedParenList l = "(\n" ++ (indent $ intercalate ",\n" l) ++ "\n)"


-- changes the usual way to display an Either type. 
showEither :: (Show a, Show b) => Either a b -> String
showEither (Left  v) = show v
showEither (Right v) = show v

-- shows a block.  If there are two blocks, they are separated by a new line.
showBlock :: (Show a, Show b) => [a] -> [b] -> String
showBlock a [] = indent $ show a
showBlock [] b = indent $ show b
showBlock a b = indent $ show a ++ '\n' : show b
