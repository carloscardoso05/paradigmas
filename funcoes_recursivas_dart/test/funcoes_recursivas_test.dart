import 'package:funcoes_recursivas/funcoes_recursivas.dart' as func;
import 'package:test/test.dart';

void main() {
  test('extract should return head and tail of list', () {
    final lista = [1, 2, 3, 4];
    final (head, tail) = func.extract(lista);
    expect(head, equals(1));
    expect(tail, equals([2, 3, 4]));
  });

  test('contains - search for elements', () {
    final lista = [29, 8, 0, 17];
    expect(func.contains(lista, 17), isTrue);
    expect(func.contains(lista, 29), isTrue);
    expect(func.contains(lista, 19), isFalse);
    expect(func.contains(lista, null), isFalse);
  });

  group('index - find element position', () {
    test('should find existing elements', () {
      final lista = [10, 20, 30, 40];
      expect(func.index(lista, 10), equals(0));
      expect(func.index(lista, 30), equals(2));
      expect(func.index(lista, 40), equals(3));
    });

    test('should return null for non-existing elements', () {
      final lista = [10, 20, 30];
      expect(func.index(lista, 50), isNull);
    });

    test('should handle empty list', () {
      expect(func.index([], 1), isNull);
    });
  });

  group('map - transform elements', () {
    test('should apply function to all elements', () {
      final lista = [1, 2, 3, 4];
      expect(func.map(lista, (x) => x * 2), equals([2, 4, 6, 8]));
    });

    test('should handle type transformations', () {
      final lista = [1, 2, 3];
      expect(func.map(lista, (x) => x.toString()), equals(['1', '2', '3']));
    });

    test('should handle empty list', () {
      expect(func.map([], (x) => x * 2), isEmpty);
    });
  });

  group('filter - select elements', () {
    test('should filter even numbers', () {
      final lista = [1, 2, 3, 4, 5, 6];
      expect(func.filter(lista, (x) => x % 2 == 0), equals([2, 4, 6]));
    });

    test('should handle empty list', () {
      expect(func.filter([], (x) => x > 0), isEmpty);
    });

    test('should handle no matches', () {
      final lista = [1, 3, 5];
      expect(func.filter(lista, (x) => x % 2 == 0), isEmpty);
    });
  });

  group('reduce - aggregate elements', () {
    test('should sum numbers', () {
      final lista = [1, 2, 3, 4];
      expect(func.reduce(lista, (acc, curr) => acc + curr, 0), equals(10));
    });

    test('should concatenate strings', () {
      final lista = ['a', 'b', 'c'];
      expect(func.reduce(lista, (acc, curr) => '$acc$curr', ''), equals('abc'));
    });

    test('should handle empty list', () {
      expect(func.reduce(<int>[], (acc, curr) => acc + curr, 0), equals(0));
    });

    test('should handle different types of list and result', () {
      expect(
        func.reduce(
          [(valor: 10), (valor: 15), (valor: 30)],
          (acc, curr) => acc + curr.valor,
          5,
        ),
        equals(60),
      );
    });
  });
}
