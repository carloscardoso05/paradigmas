menorCaminho :: [[Int]] -> Int
-- Caso da matriz vazia
menorCaminho [] = 0

-- Caso de matriz 1x1
menorCaminho [[x]] = x

-- Caso de uma única linha
menorCaminho [linha] = sum linha

-- Caso de uma única coluna
menorCaminho matriz | all (\linha -> length linha == 1) matriz = sum (map head matriz)

-- Caso geral (matriz com mais de uma linha e mais de uma coluna)
menorCaminho ((x:xs):ys) =
    x + min (menorCaminho (xs : ys)) (menorCaminho (map (x:) ys))


main :: IO ()
main = do
  let matriz = [[5, 8, 0, 7, 4, 9, 6], [3, 1, 3, 6, 1, 3, 4], [5, 2, 9, 2, 5, 0, 3], [8, 4, 0, 0, 6, 1, 5], [0, 9, 0, 8, 4, 9, 3]]
  print $ menorCaminho matriz
