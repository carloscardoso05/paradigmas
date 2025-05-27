import itertools
from typing import Literal, cast

type Caminho = list[Literal["baixo", "direita"]]
type Matriz = list[list[int]]


def extrair(lista: list):
    if not lista:
        raise ValueError("A lista não pode estar vazia.")
    return lista[0], lista[1:]


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
    _, tail = extrair(lista)
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


def printar_caminho(matriz: Matriz, caminho: Caminho):
    # Criar uma matriz de marcação do mesmo tamanho preenchida com espaços
    altura = tamanho_lista(matriz)
    largura = tamanho_lista(matriz[0]) if matriz else 0
    posicao = [0, 0]
    
    # Criar uma cópia da matriz para marcar o caminho
    matriz_caminho = []
    for i in range(altura):
        linha = []
        for j in range(largura):
            linha.append(str(matriz[i][j]).rjust(2))
        matriz_caminho.append(linha)
    
    # Marcar posição inicial
    matriz_caminho[0][0] = "→" + str(matriz[0][0]).rjust(1)
    
    # Marcar o caminho
    for movimento in caminho:
        if movimento == "baixo":
            posicao[0] += 1
            matriz_caminho[posicao[0]][posicao[1]] = "↓" + str(matriz[posicao[0]][posicao[1]]).rjust(1)
        elif movimento == "direita":
            posicao[1] += 1
            matriz_caminho[posicao[0]][posicao[1]] = "→" + str(matriz[posicao[0]][posicao[1]]).rjust(1)
    
    # Imprimir a matriz com o caminho
    print("\nCaminho na matriz:")
    for linha in matriz_caminho:
        print(" ".join(linha))
    print()


# Questão 8
def caminho_minimo(matriz: Matriz) -> Caminho:
    altura = tamanho_lista(matriz)
    largura = tamanho_lista(matriz[0]) if matriz else 0
    caminho_base = ["baixo"] * (altura - 1) + ["direita"] * (largura - 1)
    possibilidades = list(set(itertools.permutations(caminho_base)))
    custo_minimo = float("inf")
    melhor_caminho: Caminho = []
    for possibilidade in possibilidades:
        posicao = [0, 0]
        custo = matriz[0][0]
        for movimento in possibilidade:
            if custo >= custo_minimo:
                break
            if movimento == "baixo":
                posicao[0] += 1
            elif movimento == "direita":
                posicao[1] += 1
            if posicao[0] >= altura or posicao[1] >= largura:
                break
            custo += matriz[posicao[0]][posicao[1]]

        if custo < custo_minimo:
            custo_minimo = custo
            melhor_caminho = cast(Caminho, list(possibilidade))
    print("Custo mínimo:", custo_minimo)
    print("Melhor caminho:", melhor_caminho)
    printar_caminho(matriz, melhor_caminho)
    return melhor_caminho


def main():
    caminho_minimo(
        [
            [5, 8, 0, 7, 4, 9, 6],
            [3, 1, 3, 6, 1, 3, 4],
            [5, 2, 9, 2, 5, 0, 3],
            [8, 4, 0, 0, 6, 1, 5],
            [0, 9, 0, 8, 4, 9, 3]
        ]
    )
    # # Testando as funções
    # lista = [1, 2, 3, 4, 5]
    # print("Lista original:", lista)
    # print("Soma:", somar(lista))  # 15
    # print("Tamanho:", tamanho_lista(lista))  # 5
    # print("Contém 3?", contem_lista(lista, 3))  # True
    # print("Posição de 4:", posicao_lista(lista, 4))  # 3
    # print("Máximo:", maximo_lista(lista))  # 5
    # print("Lista invertida:", inverter(lista))  # [5, 4, 3, 2, 1]
    # print("Soma dos pares:", somar_pares(lista))  # 6


if __name__ == "__main__":
    main()
