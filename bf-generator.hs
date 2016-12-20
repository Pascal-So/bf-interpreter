import Data.List

diffs :: (Num a) => [a] -> [a]
diffs x = if not.null x then map (uncurry (-)) $ zip (tail x) x else []

roundtrip :: Num a => [a] -> [a]
roundtrip x = diffs (0:x ++ [0])

shift :: Int -> String
shift n = if n > 0 then replicate n '>' else replicate (-n) '<'

move :: [Int] -> String
move x = "[" ++ (intercalate "+" $ map shift $ roundtrip x) ++ "-]"




data Bf = Bf { code :: String
             , start :: Int
             , end :: Int
             }

(<+>) :: Bf -> Bf -> Bf
Bf {code=ca, start=sa, end=ea} <+> Bf {code =cb, start=sb, end=eb} =
  let
    sab = shift (sb - ea)
    merged = ca ++ sab ++ cb
  in
    Bf {code = merged, start=sa, end=sb}

