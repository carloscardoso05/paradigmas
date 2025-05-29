menorCaminho :: [[Int]] -> Int
menorCaminho [] = 0
menorCaminho matriz = 
    if length (head matriz) == 1 && length matriz == 1 -- [[1]]
        then return head (head matriz)
    if length (head matriz) == 1 -- [[1, 2, 3]]
        then return (head (head matriz) + menorCaminho (tail matriz))
    if length matriz == 1 -- [[1], [2], [3]]
        then return (head (head matriz) + menorCaminho (map tail matriz))
    else do
        let caminho_direita = head (head matriz) + menorCaminho (map tail matriz)
        let caminho_baixo = head (head (tail matriz)) + menorCaminho (tail matriz)
        return (min caminho_direita caminho_baixo)

main :: IO ()
main = do
    let matriz = [
        [5, 8, 0, 7, 4, 9, 6],
        [3, 1, 3, 6, 1, 3, 4],
        [5, 2, 9, 2, 5, 0, 3],
        [8, 4, 0, 0, 6, 1, 5],
        [0, 9, 0, 8, 4, 9, 3],
    ]
    print $ menorCaminho matriz
