```modula-2

MODULE OrdenarNumeros;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar, ReadInt;

PROCEDURE OrdenarNumeros;

  VAR
    Cantidad: CARDINAL;
    Numeros: ARRAY[0..99] OF CARDINAL;
    i, j, temp: CARDINAL;

BEGIN
  WriteString("Ingrese la cantidad de números a ordenar: ");
  Readln(Cantidad);

  WriteString("Ingrese los números a ordenar: ");
  FOR i := 0 TO Cantidad - 1 DO ReadInt(Numeros[i]); END;

  FOR i := 0 TO Cantidad - 2 DO
    FOR j := i + 1 TO Cantidad - 1 DO
      IF Numeros[j] < Numeros[i] THEN
        temp := Numeros[i];
        Numeros[i] := Numeros[j];
        Numeros[j] := temp;
      END;
    END;
  END;

  WriteString("Números ordenados: ");
  FOR i := 0 TO Cantidad - 1 DO WriteInt(Numeros[i], 0); WriteString(" "); END;

  WriteLn;
END OrdenarNumeros;

BEGIN
  OrdenarNumeros;
END OrdenarNumeros.

```

Explicación:

- El código anterior implementa un programa sencillo en Modula-2 que ordena una serie de números enteros ingresados por el usuario.

- El programa comienza declarando las variables necesarias:

  - `Cantidad`: Variable de tipo `CARDINAL` que almacena la cantidad de números que se van a ordenar.
  - `Numeros`: Arreglo de tipo `ARRAY[0..99] OF CARDINAL` que almacena los números ingresados por el usuario.
  - `i`, `j`, `temp`: Variables de tipo `CARDINAL` que se utilizan en el algoritmo de ordenamiento.

- Luego, el programa solicita al usuario que ingrese la cantidad de números a ordenar y los números en sí.

- A continuación, el programa utiliza el algoritmo de ordenamiento de burbuja para ordenar los números ingresados por el usuario. El algoritmo de ordenamiento de burbuja funciona comparando cada par de elementos adyacentes en el arreglo y, si están desordenados, los intercambia. El algoritmo repite este proceso hasta que todos los elementos del arreglo estén ordenados.

- Finalmente, el programa muestra los números ordenados en la pantalla.