# PARADIGMAS DE PROGRAMAÇÃO - ATIVIDADE 1
# Definição de funções recursivas

def cabeca[T](lista: list[T]) -> T:
    if not lista:
        raise ValueError("A lista não pode estar vazia.")
    return lista[0]


def cauda[T](lista: list[T]) -> list[T]:
    if not lista:
        raise ValueError("A lista não pode estar vazia.")
    return lista[1:]


def extrair[T](lista: list[T]) -> tuple[T, list[T]]:
    return cabeca(lista), cauda(lista)


# Questão 1
def somar(lista: list) -> float | int:
    if not lista:
        return 0
    head, tail = extrair(lista)
    return head + somar(tail)


# Questão 2
def tamanho_lista(lista: list) -> int:
    if not lista:
        return 0
    tail = cauda(lista)
    return 1 + tamanho_lista(tail)


# Questão 3
def contem_lista(lista: list, valor) -> bool:
    if not lista:
        return False
    head, tail = extrair(lista)
    if head == valor:
        return True
    return contem_lista(tail, valor)


# Questão 4
def posicao_lista(lista: list, valor) -> int | None:
    if not lista:
        return None
    head, tail = extrair(lista)
    if head == valor:
        return 0
    pos = posicao_lista(tail, valor)
    if pos is None:
        return None
    return pos + 1


# Questão 5
def maximo_lista(lista: list):
    if not lista:
        return None
    max_atual, tail = extrair(lista)
    max_tail = maximo_lista(tail)
    if max_tail is None or max_atual > max_tail:
        return max_atual
    return max_tail


# Questão 6
def inverter(lista: list) -> list:
    if not lista:
        return []
    head, tail = extrair(lista)
    return inverter(tail) + [head]


# Questão 7
def somar_pares(lista: list) -> int:
    if not lista:
        return 0
    head, tail = extrair(lista)
    if head % 2 == 0:
        return head + somar_pares(tail)
    return somar_pares(tail)

type Caminho = list[str]
type Matriz = list[list[int]]

# Questões 8 e 9
def menor_custo(
    matriz: Matriz, memo: dict[tuple[int, int], tuple[int, Caminho]] = dict()
) -> tuple[int, Caminho]:
    if matriz == [] or cabeca(matriz) == []:
        raise ValueError("A matriz não pode estar vazia.")

    x = len(cabeca(matriz))
    y = len(matriz)

    if (x, y) in memo:
        return memo[(x, y)]

    "Se a matriz tem apenas um elemento. Ex: [[1]]"
    if len(matriz) == 1 and len(cabeca(matriz)) == 1:
        return cabeca(cabeca(matriz)), []

    "Se a matriz tem apenas uma linha. Ex: [[1, 2, 3]]"
    if len(matriz) == 1:
        custo, caminho = menor_custo([cauda(cabeca(matriz))])
        custo += cabeca(cabeca(matriz))
        caminho = ["direita"] + caminho
        memo[(x, y)] = (custo, caminho)
        return custo, caminho

    """
    Se a matriz tem apenas uma coluna. Ex: [[1],
                                            [2],
                                            [3]]
    """
    if len(cabeca(matriz)) == 1:
        custo, caminho = menor_custo(cauda(matriz))
        custo += cabeca(cabeca(matriz))
        caminho = ["baixo"] + caminho
        memo[(x, y)] = (custo, caminho)
        return custo, caminho

    custo_direita, caminho_direita = menor_custo([linha[1:] for linha in matriz])
    caminho_direita = ["direita"] + caminho_direita
    custo_direita += cabeca(cabeca(matriz))
    custo_baixo, caminho_baixo = menor_custo(cauda(matriz))
    caminho_baixo = ["baixo"] + caminho_baixo
    custo_baixo += cabeca(cabeca(matriz))
    if custo_direita < custo_baixo:
        memo[(x, y)] = (custo_direita, caminho_direita)
        return custo_direita, caminho_direita
    else:
        memo[(x, y)] = (custo_baixo, caminho_baixo)
        return custo_baixo, caminho_baixo

# Funções auxiliares para testes

def gerar_matriz(n: int, m: int) -> Matriz:
    import random as rd
    if n <= 0 or m <= 0:
        raise ValueError("As dimensões da matriz devem ser maiores que zero.")
    return [[rd.randint(10, 20) for _ in range(m)] for _ in range(n)]


def printar_caminho(matriz: Matriz, caminho: Caminho):
    """Printa a matriz com o caminho marcado.
    Copia a matriz original, transforma os valores em string, troca os valores do caminho por → ou ↓, pinta o caminho de vermelho e imprime.
    """
    from colorama import Style, Back
    if not matriz or not caminho:
        print("Matriz vazia ou caminho vazio.")
        return

    matriz_copia = [list(map(lambda e: str(e) + " ", linha)) for linha in matriz]
    x, y = 0, 0

    for direcao in caminho:
        if direcao == "direita":
            matriz_copia[x][y] = (
                Back.RED + matriz_copia[x][y][:-1] + "→" + Style.RESET_ALL
            )
            y += 1
        elif direcao == "baixo":
            matriz_copia[x][y] = (
                Back.RED + matriz_copia[x][y][:-1] + "↓" + Style.RESET_ALL
            )
            x += 1

    matriz_copia[-1][-1] = Back.RED + "F" + Style.RESET_ALL

    for linha in matriz_copia:
        print(" ".join(linha))


def testar_menor_custo(matriz: Matriz) -> tuple[int, Caminho]:
    import time

    tempo_inicial = time.time()
    custo, caminho = menor_custo(matriz)
    tempo_final = time.time()
    print("\nMatriz:")
    for linha in matriz:
        print(linha)
    print(f"Custo mínimo: {custo}")
    print(f"Caminho: {caminho}")
    printar_caminho(matriz, caminho)
    print(f"Tempo de execução: {tempo_final - tempo_inicial:.6f} segundos")
    return custo, caminho


def main():
    lista = [1, 2, 3, 4, 5]
    print("Lista original:", lista)
    print("Soma:", somar(lista))  # 15
    print("Tamanho:", tamanho_lista(lista))  # 5
    print("Contém 3?", contem_lista(lista, 3))  # True
    print("Posição de 4:", posicao_lista(lista, 4))  # 3
    print("Máximo:", maximo_lista(lista))  # 5
    print("Lista invertida:", inverter(lista))  # [5, 4, 3, 2, 1]
    print("Soma dos pares:", somar_pares(lista))  # 6

    matriz_teste1 = [
        [5, 8, 0, 7, 4, 9, 6],
        [3, 1, 3, 6, 1, 3, 4],
        [5, 2, 9, 2, 5, 0, 3],
        [8, 4, 0, 0, 6, 1, 5],
        [0, 9, 0, 8, 4, 9, 3],
    ]
    matriz_teste2 = gerar_matriz(10, 15)
    testar_menor_custo(matriz_teste1)
    testar_menor_custo(matriz_teste2)


if __name__ == "__main__":
    main()
