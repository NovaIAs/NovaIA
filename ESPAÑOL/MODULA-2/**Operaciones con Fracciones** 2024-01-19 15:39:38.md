```modula-2
MODULE Fraccionarios;
FROM Sort IMPORT MergeSort;
FROM Streams IMPORT WriteF, WriteLn, WriteS, WriteU;

TYPE
  Fraccion = RECORD
    entero: CARDINAL;
    numerador: CARDINAL;
    denominador: CARDINAL;
  END;

PROCEDURE Normaliza(VAR f: Fraccion);
BEGIN
  IF f.denominador = 0 THEN
    f.numerador := 0;
  ELSE
    VAR mcd: CARDINAL;
    mcd := MCD(f.numerador, f.denominador);
    f.numerador := f.numerador DIV mcd;
    f.denominador := f.denominador DIV mcd;
  END;
END Normaliza;

PROCEDURE Suma(VAR f1, f2: Fraccion; OUT resultado: Fraccion);
BEGIN
  resultado.entero := f1.entero + f2.entero;
  resultado.numerador := f1.numerador * f2.denominador + f2.numerador * f1.denominador;
  resultado.denominador := f1.denominador * f2.denominador;
  Normaliza(resultado);
END Suma;

PROCEDURE Resta(VAR f1, f2: Fraccion; OUT resultado: Fraccion);
BEGIN
  resultado.entero := f1.entero - f2.entero;
  resultado.numerador := f1.numerador * f2.denominador - f2.numerador * f1.denominador;
  resultado.denominador := f1.denominador * f2.denominador;
  Normaliza(resultado);
END Resta;

PROCEDURE Multiplica(VAR f1, f2: Fraccion; OUT resultado: Fraccion);
BEGIN
  resultado.entero := f1.entero * f2.entero;
  resultado.numerador := f1.numerador * f2.numerador;
  resultado.denominador := f1.denominador * f2.denominador;
  Normaliza(resultado);
END Multiplica;

PROCEDURE Divide(VAR f1, f2: Fraccion; OUT resultado: Fraccion);
BEGIN
  resultado.entero := f1.entero DIV f2.entero;
  resultado.numerador := f1.numerador * f2.denominador;
  resultado.denominador := f1.denominador * f2.numerador;
  Normaliza(resultado);
END Divide;

PROCEDURE Imprime(f: Fraccion);
BEGIN
  WriteU(f.entero, 0);
  WriteS(" ");
  WriteU(f.numerador, 0);
  WriteS("/");
  WriteU(f.denominador, 0);
  WriteLn;
END Imprime;

PROCEDURE MCD(a, b: CARDINAL): CARDINAL;
BEGIN
  VAR resto: CARDINAL;
  WHILE b /= 0 DO
    resto := a MOD b;
    a := b;
    b := resto;
  END;
  RETURN a;
END MCD;

VAR fracciones: ARRAY 10 OF Fraccion;
VAR i: CARDINAL;

BEGIN
  FOR i := 0 TO HIGH(fracciones) DO
    WriteS("Fracción ");
    WriteU(i+1, 0);
    WriteS(": ");
    ReadF(fracciones[i].entero, 0);
    WriteS(" ");
    ReadF(fracciones[i].numerador, 0);
    WriteS("/");
    ReadF(fracciones[i].denominador, 0);
    WriteLn;
  END;

  MergeSort(fracciones);

  WriteS("Fracciones ordenadas:");
  WriteLn;
  FOR i := 0 TO HIGH(fracciones) DO
    Imprime(fracciones[i]);
  END;

  WriteLn;
  WriteS("Suma de las fracciones:");
  WriteLn;
  Imprime(fracciones[0]);
  WriteS(" + ");
  Imprime(fracciones[1]);
  WriteS(" = ");
  VAR resultado: Fraccion;
  Suma(fracciones[0], fracciones[1], resultado);
  Imprime(resultado);

  WriteLn;
  WriteS("Resta de las fracciones:");
  WriteLn;
  Imprime(fracciones[0]);
  WriteS(" - ");
  Imprime(fracciones[1]);
  WriteS(" = ");
  Resta(fracciones[0], fracciones[1], resultado);
  Imprime(resultado);

  WriteLn;
  WriteS("Multiplicación de las fracciones:");
  WriteLn;
  Imprime(fracciones[0]);
  WriteS(" * ");
  Imprime(fracciones[1]);
  WriteS(" = ");
  Multiplica(fracciones[0], fracciones[1], resultado);
  Imprime(resultado);

  WriteLn;
  WriteS("División de las fracciones:");
  WriteLn;
  Imprime(fracciones[0]);
  WriteS(" / ");
  Imprime(fracciones[1]);
  WriteS(" = ");
  Divide(fracciones[0], fracciones[1], resultado);
  Imprime(resultado);
END Fraccionarios.
```

Explicación:

* El código define primero el tipo de dato `Fraccion`, que es un registro que contiene tres campos: `entero`, `numerador` y `denominador`.
* A continuación, se definen una serie de procedimientos para operar con fracciones: `Normaliza`, `Suma`, `Resta`, `Multiplica` y `Divide`.
* También se define un procedimiento `Imprime` para imprimir fracciones.
* Se declara un array de 10 fracciones y se leen los valores de las fracciones desde el teclado.
* Se ordenan las fracciones usando el algoritmo de ordenación por fusión.
* Se imprimen las fracciones ordenadas, se calcula y se imprime la suma, la resta, la multiplicación y la división de las primeras dos fracciones.

El algoritmo de ordenación por fusión es el siguiente:

* Dividir el array en dos subarrays de igual tamaño.
* Ordenar recursivamente cada subarray.
* Fusionar los dos subarrays ordenados en un único array ordenado.

La complejidad del algoritmo de ordenación por fusión es O(n log n), donde n es el tamaño del array.