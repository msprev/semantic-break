import Data.List
import Data.List.Split
import NLP.FullStop

main :: IO ()
main = interact (unlines . unparas . parasProcess . lines)

-- | split into paragraphs and process each individually
parasProcess :: [String] -> [[String]]
parasProcess x = map process (splitWhen (=="") x)
    where
      process [] = []
      process p
        | head p == "---" = p
        | otherwise = (indent . semanticBreak . removeNewline) p

-- | join paragraphs
unparas :: [[String]] -> [String]
unparas = intercalate [""]

-- | flatten lines to single line
removeNewline :: [String] -> [String]
removeNewline = lines . intercalate " "

-- | break into lines on sentence endings
semanticBreak :: [String] -> [String]
semanticBreak = foldl accRule []
    where accRule acc x = (NLP.FullStop.segment x ++ acc)

-- | indent all but first line by 4 spaces
indent :: [String] -> [String]
indent []     = []
indent (x:xs) = x:(map pushright xs)
    where pushright y = "    " ++ y
