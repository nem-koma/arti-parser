import Data.List
import Text.Regex.PCRE
import Data.Maybe

import Configs

<<<<<<< HEAD
=======
-- lines :: String -> [String]の抽象
-- fragments         :: Eq a => a -> [a] -> [[a]]
-- fragments del str = cons (case break (== del) str of
	-- (left, str') -> (left, case str' of
		-- []       -> []
		-- _:str''  -> fragments del str''))
	-- where cons ~(h, t) = h : t

>>>>>>> 2efc570
-- いずれかの構文である (第一引数の関数群のすくなくとも一つが真を返す)
is_any :: [String -> Bool] -> String -> Bool
is_any syntaxes str = any ($ str) syntaxes

-- ss, cs = syntaxes, clustersyntaxes
clustering :: [String] -> [(String,String)] -> [String] -> [String]
clustering ss cs strs = case span (is_any bools) strs of
	(left, right) -> left ++ (case right of
		[]       -> []
		l:right' | isJust isn      -> if null a then intercalate "\n" cl : (clustering ss cs (drop 1 cr))
							else intercalate "\n" (head a : cl) : (clustering ss cs (drop 1 cr))
			 | otherwise       -> intercalate "" (l : l') : clustering ss cs r'
			where   isn          = find (\x -> (fst x) /=/ l) cs
				~n           = fromJust isn
				~(_,_,_,a)   = (fst n) /=/ l :: (String,String,String,[String])

				~(cl, cr)    = break ((/=/) (snd n)) right'
				~(l', r')    = break (is_any $ bools ++ csfst) right') -- クラスタを含む構文に一致しない範囲とそれ以降
	where   csfst = map (/=/) $ map fst cs
		bools = map (/=/) ss
-- ~(cl, cr) = b reak ((/=/)は=~から変えたのでflipが必要そう 何故か挙動がかわらない


main = do
	c <- getContents
	let ls = lines c
	let clustered = clustering (map fst syntaxes_def) bsyntaxes_def ls
	let mapped = map (syntax_mapping syntaxes_def) clustered
	putStr $ intercalate "\n" mapped ++ "\n"

