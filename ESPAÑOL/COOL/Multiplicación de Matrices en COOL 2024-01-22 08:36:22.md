```cool
clase Matriz {
    atributo Entero[,] datos;
    entero fila, columna;

    constructor crear(fila: Entero, columna: Entero) {
        self.fila := fila;
        self.columna := columna;
        self.datos := new Entero[fila, columna];
    }

    método set(fila: Entero, columna: Entero, valor: Entero) {
        self.datos[fila, columna] := valor;
    }

    método get(fila: Entero, columna: Entero): Entero {
        return self.datos[fila, columna];
    }

    método imprimir() {
        for (fila = 0; fila < self.fila; fila++) {
            for (columna = 0; columna < self.columna; columna++) {
                print(self.datos[fila, columna], " ");
            }
            print();
        }
    }
}

clase MultiplicarMatrices {
    atributo Matriz matriz1, matriz2, matriz_resultado;

    constructor crear(matriz1: Matriz, matriz2: Matriz) {
        self.matriz1 := matriz1;
        self.matriz2 := matriz2;

        if (matriz1.columna != matriz2.fila) {
            error("No se pueden multiplicar matrices con estas dimensiones.");
        } else {
            self.matriz_resultado := new Matriz.crear(matriz1.fila, matriz2.columna);
        }
    }

    método calcular() {
        for (fila = 0; fila < self.matriz_resultado.fila; fila++) {
            for (columna = 0; columna < self.matriz_resultado.columna; columna++) {
                suma := 0;
                for (i = 0; i < self.matriz1.columna; i++) {
                    suma += self.matriz1.get(fila, i) * self.matriz2.get(i, columna);
                }
                self.matriz_resultado.set(fila, columna, suma);
            }
        }
    }

    método imprimir_resultado() {
        self.matriz_resultado.imprimir();
    }
}

clase Principal {
    método main() {
        matriz1 := new Matriz.crear(2, 3);
        matriz2 := new Matriz.crear(3, 2);

        matriz1.set(0, 0, 1);
        matriz1.set(0, 1, 2);
        matriz1.set(0, 2, 3);
        matriz1.set(1, 0, 4);
        matriz1.set(1, 1, 5);
        matriz1.set(1, 2, 6);

        matriz2.set(0, 0, 7);
        matriz2.set(0, 1, 8);
        matriz2.set(1, 0, 9);
        matriz2.set(1, 1, 10);
        matriz2.set(2, 0, 11);
        matriz2.set(2, 1, 12);

        multiplicar_matrices := new MultiplicarMatrices.crear(matriz1, matriz2);
        multiplicar_matrices.calcular();
        multiplicar_matrices.imprimir_resultado();
    }
}
```

Este código crea dos matrices, las multiplica y luego imprime el resultado.

La clase `Matriz` define una matriz genérica que puede almacenar cualquier tipo de datos. La clase `MultiplicarMatrices` define un objeto que puede multiplicar dos matrices. La clase `Principal` define el método `main` que crea las dos matrices, las multiplica e imprime el resultado.

El código es complejo porque utiliza varias clases y métodos. También utiliza la sintaxis de COOL, que es un lenguaje de programación orientado a objetos.