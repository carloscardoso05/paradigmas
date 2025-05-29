from collections.abc import Callable


def extract[T](lista: list[T]) -> tuple[T, list[T]]:
    if not lista:
        raise ValueError(f"Lista não pode ser vazia ou None. Valor dado {lista}")
    return lista[0], lista[1:]


def index[T](lista: list[T], valor: T) -> int | None:
    if not lista:
        return None
    head, tail = extract(lista)
    if head == valor:
        return 0
    posicao = index(tail, valor)
    if posicao is None:
        return None
    return posicao + 1


def contains[T](lista: list[T], valor: T) -> bool:
    if not lista:
        return False
    head, tail = extract(lista)
    if head == valor:
        return True
    return contains(tail, valor)


def map_list[T, U](lista: list[T], funcao: Callable[[T], U]) -> list[U]:
    if not lista:
        return []
    head, tail = extract(lista)
    return [funcao(head)] + map_list(tail, funcao)


def filter_list[T](lista: list[T], funcao: Callable[[T], bool]) -> list[T]:
    if not lista:
        return []
    head, tail = extract(lista)
    if funcao(head):
        return [head] + filter_list(tail, funcao)
    return filter_list(tail, funcao)


def reduce_list[T, U](
    lista: list[T], funcao: Callable[[U, T], U], valor_inicial: U
) -> U:
    if not lista:
        return valor_inicial
    head, tail = extract(lista)
    return reduce_list(tail, funcao, funcao(valor_inicial, head))