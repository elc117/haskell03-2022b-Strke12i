add10toall::[Int] -> [Int]
add10toall x = map (\i -> i + 10) x

multN :: Int -> [Int] -> [Int]
multN n x = [i * n | i <- x]

multN' :: Int -> [Int] -> [Int]
multN' n x = map (\i -> i * n) x

applyExpr :: [Int] -> [Int]
applyExpr x = [(i * 3)+2 | i <- x]

applyExpr' :: [Int] -> [Int]
applyExpr' x = map (\i -> (i * 3) + 2 ) x

addSuffix :: String -> [String] -> [String]
addSuffix s x = [i ++ s | i <- x]

selectgt5 :: [Int] -> [Int]
selectgt5 x = [i | i <- x, i > 5]

sumOdds :: [Int] -> Int
sumOdds x = sum[ i | i <- x, (i `mod` 2) == 1]

sumOdds' :: [Int] -> Int
sumOdds' x = sum (filter (\i -> (i `mod` 2) == 1) x)

selectExpr :: [Int] -> [Int]
selectExpr x = [i | i <-x , ((i `mod` 2) == 0) && (i > 20 && i < 50)]

calcExpr :: [Float] -> [Float]
calcExpr x = [f | f <- [ i ^ 2 / 2 | i <-x ], f >= 10]

countShorts :: [String] -> Int 
countShorts x = length [ i | i <- x, length i < 5]

trSpaces :: String -> String
trSpaces x = [ if i == ' ' then '-' else i | i <- x]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd x = [ snd i | i <- x]

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum[ (fst i ) * (snd i) | i <- (zip x y)]

main = do 
  print(add10toall [0,1,2])
  print(multN 3 [1,2,3])
  print(multN' 3 [1,2,3])
  print(applyExpr [1,2,3])
  print(applyExpr' [1,2,3])
  print(addSuffix "@inf.ufsm.br" ["fulano","beltrano"])
  print(sumOdds [1,2,3,4,5,6,7])
  print(sumOdds' [1,2,3,4,5,6,7])
  print(selectExpr [21,3,56,34,5,89,12,33,24])
  print(calcExpr [1,4,5,2,7,8])
  print(countShorts ["Hi","Testee","I","OOOOOOOOOOOO"])
  print(trSpaces "Hello World Mundo")
  print(selectSnd [(1,2),(3,4),(5,6)])
  print(dotProd [1,1,1,1] [2,2,2,2])
