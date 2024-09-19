module Configs where

import Data.List
import Data.Maybe
import Text.Regex.PCRE

-- 一致する構文を適用していくので、一致するかは判別する必要がある fst syntaxes_defを再利用する
syntax_mapping :: [(String, (String -> String))] -> String -> String
syntax_mapping synmap str
	| isJust is     = snd (fromJust is) str
	| isNothing is  = intercalate (inlines_apply str) ["<p>", "</p>"]
	where is        = find (\(f,_) -> f /=/ str) synmap

-- デフォルトがマルチラインモード 改行文字を取れないので再定義
(/=/) :: RegexContext Regex source target => String -> source -> target
(/=/) r = match reg
	where   reg = makeRegexOpts blankCompOpt defaultExecOpt r :: Regex

-- 構文とクラスタ指定子の定義リスト 変換時にも必要なので変換用の関数とのタプル
-- fstはBoolに使うので、関数がMaybeを返すようにすれば除外できる
syntaxes_def :: [(String, (String -> String))]
syntaxes_def =  [ (r, f r)
		| (r, f) <- [ ("^(={1,5})[ \n](.*)$",                   heading)
			    , ("^link:\\[\\[(.*?)\\]\\[(.*?)\\]\\]$",   hyperlink)
			    , ("^\\[src\\]\\[(.*?)\\]\\s([\\s\\S]*)",   source_node)
			    , ("^$",                                    empty_line)]]

-- (start,end)
bsyntaxes_def :: [(String, String)]
bsyntaxes_def = [ (("^----$"),                  ("^----$"))
		, (("^====$|^====\\[(.*)\\]$"), ("^====$"))
		, (("^____$|^____(.*)$"),       ("^____$"))]

inlines_apply :: String -> String
inlines_apply = foldl1 (.) inlines_def

-- インライン定義リスト
inlines_def :: [String -> String]
inlines_def = [ inlines r a
	  | (r,a) <- [ ("~~(.*?)~~", ("<code>", "</code>"))
		     , ("__(.*?)__", ("<u>", "</u>"))]]

-- ネストの処理 もっと綺麗にかけそう
inlines :: String -> (String,String) -> String -> String
inlines regex (s, e) str
	| str       == [] = ""
	| ms        == [] = b ++ a
	| otherwise       = b ++ s ++ (head ms) ++ e ++ inlines regex (s, e) a
	where (b,m,a,ms)  = regex /=/ str :: (String,String,String,[String])

-- 適用する関数群の定義と適用
empty_line :: String -> String -> String
empty_line reg str = ""

-- ネスト要素はinlines_applyを文字列部分に適用する:
-- paragraph :: String -> String
-- paragraph str = intercalate "" ["<p>", inlines_apply str, "</p>"]

heading :: String -> String -> String
heading reg str = intercalate ss tag
	where   (_,_,_,(s:ss:_)) = reg /=/ str :: (String,String,String,[String])
		depth            = show $ length s
		tag              = [ '<':x++'h':depth++">" | x <- subsequences ['/']]

hyperlink :: String -> String -> String
hyperlink reg str = tag
	where   (_,_,_,(text:link:_)) = reg /=/ str :: (String,String,String,[String])
		tag                   = "<a href='" ++ link ++ "'>" ++ text ++ "</a>"

source_node :: String -> String -> String
source_node reg str = tag
	where   (_,_,_,(l:code:_)) = reg /=/ str :: (String,String,String,[String])
		tag                  = intercalate "\n" ["<pre><code>", code, "</code></pre>"]
