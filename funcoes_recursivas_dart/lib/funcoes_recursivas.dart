(T, List<T>) extract<T>(List<T> lista) {
  return (lista.first, lista.sublist(1));
}

bool contains<T>(List<T> lista, T valor) {
  if (lista.isEmpty) return false;
  final (head, tail) = extract(lista);
  if (head == valor) return true;
  return contains(tail, valor);
}

int? index<T>(List<T> lista, T valor) {
  if (lista.isEmpty) return null;
  final (head, tail) = extract(lista);
  if (head == valor) return 0;
  final posicao = index(tail, valor);
  if (posicao == null) return null;
  return posicao + 1;
}

List<U> map<T, U>(List<T> lista, U Function(T) funcao) {
  if (lista.isEmpty) return [];
  final (head, tail) = extract(lista);
  return [funcao(head)] + map(tail, funcao);
}

List<T> filter<T>(List<T> lista, bool Function(T) funcao) {
  if (lista.isEmpty) return [];
  final (head, tail) = extract(lista);
  if (funcao(head)) {
    return [head] + filter(tail, funcao);
  }
  return filter(tail, funcao);
}

U reduce<T, U>(List<T> lista, U Function(U, T) funcao, U valorInicial) {
  if (lista.isEmpty) return valorInicial;
  final (head, tail) = extract(lista);
  final acc = funcao(valorInicial, head);
  return reduce(tail, funcao, acc);
}
