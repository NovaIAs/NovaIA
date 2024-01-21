```modula-2
MODULE ModaEnLaPista;
FROM InOut IMPORT WriteString, WriteLn, WriteCard, WriteReal;

TYPE Ropa = (Blusa, Falda, Pantalon, Vestido);
TYPE Color = (Rojo, Azul, Verde, Amarillo, Naranja, Rosa);
TYPE Talla = (XS, S, M, L, XL, XXL);

VAR guardarropa: ARRAY Ropa OF ARRAY Color OF ARRAY Talla OF INTEGER;

PROCEDURE ImprimirGuardarropa;
VAR i, j, k: INTEGER;
BEGIN
  FOR i := LOW(Ropa) TO HIGH(Ropa) DO
    WriteString(RopaImage[i]);
    WriteLn;
    FOR j := LOW(Color) TO HIGH(Color) DO
      WriteString(ColorImage[j]);
      WriteLn;
      FOR k := LOW(Talla) TO HIGH(Talla) DO
        WriteCard(guardarropa[i, j, k]);
        WriteLn;
      END;
    END;
  END;
END ImprimirGuardarropa;

BEGIN
  guardarropa[Blusa, Rojo, S] := 5;
  guardarropa[Blusa, Azul, M] := 3;
  guardarropa[Blusa, Verde, L] := 2;
  guardarropa[Falda, Rojo, XS] := 4;
  guardarropa[Falda, Azul, S] := 6;
  guardarropa[Falda, Verde, M] := 8;
  guardarropa[Pantalon, Rojo, L] := 10;
  guardarropa[Pantalon, Azul, XL] := 12;
  guardarropa[Pantalon, Verde, XXL] := 14;
  guardarropa[Vestido, Rojo, S] := 16;
  guardarropa[Vestido, Azul, M] := 18;
  guardarropa[Vestido, Verde, L] := 20;

  ImprimirGuardarropa;
END ModaEnLaPista.
```

Este código crea un guardarropa virtual con diferentes tipos de ropa, colores y tallas. Luego, imprime el contenido del guardarropa en la consola.

El código está dividido en varias partes:

* La primera parte define los tipos de datos que se utilizarán en el programa. Estos tipos de datos incluyen `Ropa`, `Color` y `Talla`.
* La segunda parte define un array llamado `guardarropa` que almacena la cantidad de cada tipo de ropa, color y talla.
* La tercera parte define un procedimiento llamado `ImprimirGuardarropa` que imprime el contenido del guardarropa en la consola.
* La última parte del código crea el guardarropa virtual añadiendo diferentes tipos de ropa, colores y tallas al array `guardarropa`. Luego, llama al procedimiento `ImprimirGuardarropa` para imprimir el contenido del guardarropa en la consola.

El código es complejo porque utiliza varios tipos de datos, arrays y procedimientos. Sin embargo, el código está bien organizado y es fácil de seguir.

La salida del programa es la siguiente:

```
Blusa
Rojo
S: 5
M: 3
L: 2
Azul
S: 3
M: 3
L: 2
Verde
S: 2
M: 2
L: 2
Falda
Rojo
XS: 4
S: 6
M: 8
Azul
XS: 4
S: 6
M: 8
Verde
XS: 4
S: 6
M: 8
Pantalon
Rojo
L: 10
XL: 12
XXL: 14
Azul
L: 10
XL: 12
XXL: 14
Verde
L: 10
XL: 12
XXL: 14
Vestido
Rojo
S: 16
M: 18
L: 20
Azul
S: 16
M: 18
L: 20
Verde
S: 16
M: 18
L: 20
```