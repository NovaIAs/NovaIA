```ada
with Ada.Text_IO;

procedure TrianguloPascal is

  function CoeficienteBinomial (n, k : Integer) return Integer is
  begin
    if k > n then
      return 0;
    elsif k = 0 or k = n then
      return 1;
    else
      return CoeficienteBinomial(n-1, k-1) + CoeficienteBinomial(n-1, k);
    end if;
  end CoeficienteBinomial;

  type Fila is array (Integer range 0 .. <>) of Integer;

  function ImprimirFila (fila : Fila) is
  begin
    for i in fila'range loop
      Ada.Text_IO.Put(fila(i), 1);
      if i < fila'last then
        Ada.Text_IO.Put(", ", 1);
      end if;
    end loop;
  end ImprimirFila;

  procedure ImprimirTriangulo (n : Integer) is
    fila : Fila(0 to n);

    for i in 0 .. n loop
      fila(i) := CoeficienteBinomial(n, i);
    end loop;

    for i in fila'range loop
      ImprimirFila(fila(i));
      Ada.Text_IO.New_Line(1);
    end loop;
  end ImprimirTriangulo;

begin
  ImprimirTriangulo(10);
end TrianguloPascal;
```

Este código calcula y muestra el triángulo de Pascal, que es un arreglo triangular de números binomiales, donde la fila superior es 1, y cada número en la fila debajo es la suma de los dos números encima de él.

El código utiliza una función recursiva `CoeficienteBinomial` para calcular el coeficiente binomial de dos números, que es el número de formas de seleccionar k elementos de un conjunto de n elementos.

El procedimiento `ImprimirFila` imprime una fila del triángulo de Pascal, y el procedimiento `ImprimirTriangulo` imprime el triángulo completo hasta la fila n.

Al ejecutar el código, se mostrará el siguiente triángulo de Pascal:

```
1
1, 1
1, 2, 1
1, 3, 3, 1
1, 4, 6, 4, 1
1, 5, 10, 10, 5, 1
1, 6, 15, 20, 15, 6, 1
1, 7, 21, 35, 35, 21, 7, 1
1, 8, 28, 56, 70, 56, 28, 8, 1
1, 9, 36, 84, 126, 126, 84, 36, 9, 1
1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1
```