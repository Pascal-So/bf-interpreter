import Data.List

diffs :: (Num a) => [a] -> [a]
diffs x = if (not.null) x then map (uncurry (-)) $ zip (tail x) x else []

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

instance Show Bf where
  show Bf{code=c, start=s} =
    let
      sft = shift (s)
    in
      sft ++ c

instance Read Bf where
  readsPrec _ s =
    let
      up = length $ filter (=='>') s
      down = length $ filter (=='<') s
      diffLvl = up-down
    in
      [(Bf{code=s, start=0, end=diffLvl},"")]
  

(<+>) :: Bf -> Bf -> Bf
Bf {code=ca, start=sa, end=ea} <+> Bf {code =cb, start=sb, end=eb} =
  let
    sft = shift (sb - ea)
    merged = ca ++ sft ++ cb
  in
    Bf {code = merged, start=sa, end=sb}

loopOn :: Int -> Bf -> Bf
loopOn x Bf{code=body, start=s, end=e} =
  let
    sft = shift(s - x)
    sft_back = shift(x - e)
    loopcode = "[" ++ sft ++ body ++ sft_back ++ "-]"
  in
    Bf {code=loopcode, start=x, end=x}



test :: Bf
test = Bf {code=(replicate 5 '+'), start=0,end=0} <+> (loopOn 0 $ Bf{code="+", start=1, end=1})

main = putStrLn $ show test
