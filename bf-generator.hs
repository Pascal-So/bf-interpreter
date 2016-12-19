intsp :: a ++ [[a]] -> [a]
intsp a lst =
  tail $ foldr (\x acc -> a:x:acc) [] lst


diffs :: (Num a) => [a] -> [a]
diffs x = if not.null $ x then map (uncurry (-)) $ zip (tail x) x else []

roundtrip :: Num a => [a] -> [a]
roundtrip x = diffs (0:x ++ [0])

move :: [Int] -> String
move x = "[" ++ (intsp '+' $ map shift $ roundtrip x) ++ "-]"
