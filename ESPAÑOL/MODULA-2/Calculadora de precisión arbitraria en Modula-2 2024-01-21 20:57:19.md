```modula-2
MODULE Calculadora;

IMPORT
  Format,
  LongInt,
  LongReal,
  TextIO;

TYPE
  LongLongInt = ARRAY[0..15] OF INTEGER;

PROCEDURE Añadir(VAR Res: LongLongInt; A, B: LongReal);

VAR
  -- Los resultados parciales se almacenan en una matriz.
  Res: LongLongInt;

BEGIN
  -- Convertir los operandos en enteros largos.
  ~A := A * LongReal(1 shl 32);
  ~B := B * LongReal(1 shl 32);

  -- Sumar los operandos.
  FOR i := 0 TO 15 DO
    Res[i] := ~A[i] + ~B[i];
  END;

  -- Corregir cualquier desbordamiento.
  FOR i := 0 TO 14 DO
    IF Res[i] > 0ffffffffh THEN
      Res[i + 1] := Res[i + 1] + 1;
      Res[i] := Res[i] - 100000000h;
    END;
  END;
END Añadir;

PROCEDURE Quitar(VAR Res: LongLongInt; A, B: LongReal);

VAR
  -- Los resultados parciales se almacenan en una matriz.
  Res: LongLongInt;

BEGIN
  -- Convertir los operandos en enteros largos.
  ~A := A * LongReal(1 shl 32);
  ~B := B * LongReal(1 shl 32);

  -- Restar los operandos.
  FOR i := 0 TO 15 DO
    Res[i] := ~A[i] - ~B[i];
  END;

  -- Corregir cualquier desbordamiento.
  FOR i := 0 TO 14 DO
    IF Res[i] < 0 THEN
      Res[i + 1] := Res[i + 1] - 1;
      Res[i] := Res[i] + 100000000h;
    END;
  END;
END Quitar;

PROCEDURE Multiplicar(VAR Res: LongLongInt; A, B: LongReal);

VAR
  -- Los resultados parciales se almacenan en una matriz.
  Res: LongLongInt;

  -- Productos intermedios.
  Temp: ARRAY[0..15] OF LongReal;

BEGIN
  -- Convertir los operandos en enteros largos.
  ~A := A * LongReal(1 shl 32);
  ~B := B * LongReal(1 shl 32);

  -- Multiplicar los operandos.
  FOR i := 0 TO 15 DO
    FOR j := 0 TO 15 DO
      Temp[i + j] := Temp[i + j] + ~A[i] * ~B[j];
    END;
  END;

  -- Corregir cualquier desbordamiento.
  FOR i := 0 TO 30 DO
    IF Temp[i] > 0ffffffffh THEN
      Temp[i + 1] := Temp[i + 1] + 1;
      Temp[i] := Temp[i] - 100000000h;
    END;
  END;

  -- Convertir los productos intermedios en enteros largos.
  FOR i := 0 TO 15 DO
    Res[i] := 0;
    FOR j := 0 TO i DO
      Res[i] := Res[i] + LongInt(Temp[i - j]);
    END;
  END;
END Multiplicar;

PROCEDURE Dividir(VAR Dividendo, Divisor: LongLongInt; VAR Cociente, Resto: LongLongInt);

VAR
  -- El dividendo y el divisor se almacenan en matrices.
  Dividendo: LongLongInt;
  Divisor: LongLongInt;

  -- El cociente y el resto se almacenan en matrices.
  Cociente: LongLongInt;
  Resto: LongLongInt;

  -- Variables temporales.
  Temp: ARRAY[0..15] OF LongReal;
  Temp2: ARRAY[0..15] OF LongReal;
  Temp3: LongLongInt;

BEGIN
  -- Convertir el dividendo y el divisor en números de punto fijo.
  FOR i := 0 TO 15 DO
    Temp[i] := Dividendo[i] * LongReal(1 shl 32);
    Temp2[i] := Divisor[i] * LongReal(1 shl 32);
  END;

  -- Inicializar el cociente y el resto.
  ~Cociente := 0;
  ~Resto := ~Dividendo;

  -- Realizar la división.
  FOR i := 15 DOWNTO 0 DO
    IF Temp[i] >= Temp2[i] THEN
      -- Restar el divisor del dividendo.
      Temp3 := ~Resto;
      FOR j := 0 TO 15 DO
        Temp3[j] := Temp3[j] - Temp2[j];
      END;

      -- Corregir cualquier desbordamiento.
      FOR j := 0 TO 14 DO
        IF Temp3[j] < 0 THEN
          Temp3[j + 1] := Temp3[j + 1] - 1;
          Temp3[j] := Temp3[j] + 100000000h;
        END;
      END;

      -- Aumentar el cociente.
      ~Cociente := ~Cociente + 1;

      -- Actualizar el dividendo.
      ~Resto := ~Temp3;
    END;
  END;

  -- Convertir el cociente y el resto en enteros largos.
  FOR i := 0 TO 15 DO
    Cociente[i] := 0;
    Resto[i] := 0;
    FOR j := 0 TO i DO
      Cociente[i] := Cociente[i] + LongInt(Temp[i - j]);
      Resto[i] := Resto[i] + LongInt(Temp3[i - j]);
    END;
  END;
END Dividir;

PROCEDURE Potencia(VAR Res: LongLongInt; A: LongReal; N: LONGINT);

VAR
  -- El resultado se almacena en una matriz.
  Res: LongLongInt;

  -- Variable temporal.
  Temp: LongLongInt;

BEGIN
  -- Convertir el operando en un número de punto fijo.
  Temp := A * LongReal(1 shl 32);

  -- Inicializar el resultado.
  ~Res := 1;

  -- Calcular la potencia mediante el método de la división repetida por 2.
  WHILE N > 0 DO
    IF N MOD 2 = 1 THEN
      ~Res := ~Res * ~Temp;
    END;
    ~Temp := ~Temp * ~Temp;
    N := N / 2;
  END;
END Potencia;

PROCEDURE RaízCuadrada(VAR Res: LongReal; N: LongReal);

VAR
  -- El resultado se almacena en un número de punto fijo.
  Res: LongReal;

  -- Variable temporal.
  Temp: ARRAY[0..15] OF LongReal;

BEGIN
  -- Convertir el operando en un número de punto fijo.
  FOR i := 0 TO 15 DO
    Temp[i] := N * LongReal(1 shl 32);
  END;

  -- Calcular la raíz cuadrada mediante el método de la iteración babilónica.
  FOR i := 0 TO 100 DO
    Res := N / Res;
    Res := (Res + N / Res) / LongReal(2);
  END;

  -- Convertir el resultado en un número de punto flotante.
  Res := Res / LongReal(1 shl 32);
END RaízCuadrada;

PROCEDURE MostrarResultados(Op1, Op2, Res: LongReal);

BEGIN
  TextIO.Put(Op1, 15, 1);
  TextIO.Put(Format.String(" %c ", Op), 3);
  TextIO.Put(Op2, 15, 1);
  TextIO.Put(Format.String(" = %18.6f", Res), 1, 20);
  TextIO.Newline;
END MostrarResultados;

BEGIN
  -- Declarar las variables.
  VAR
    Op1, Op2, Res: LongReal;
    Op: CHAR;

  -- Leer los operandos y la operación.
  TextIO.Put("Introduzca el primer operando: ");
  TextIO.ReadLongReal(Op1);
  TextIO.Newline;
  TextIO.Put("Introduzca el segundo operando: ");
  TextIO.ReadLongReal(Op2);
  TextIO.Newline;
  TextIO.Put("Introduzca la operación (+, -, *, /, ^, sqrt): ");
  TextIO.ReadChar(Op);
  TextIO.Newline;

  -- Realizar la operación correspondiente.
  CASE Op OF
    '+': Añadir(Res, Op1, Op2);
    '-': Quitar(Res, Op1, Op2);
    '*': Multiplicar(Res, Op1, Op2);
    '/': Dividir(Op1, Op2, Res, Res);
    '^': Potencia(Res, Op1, Op2);
    'S': RaízCuadrada(Res, Op1);
  END;

  -- Mostrar los resultados.
  MostrarResultados(Op1, Op2, Res);
END.