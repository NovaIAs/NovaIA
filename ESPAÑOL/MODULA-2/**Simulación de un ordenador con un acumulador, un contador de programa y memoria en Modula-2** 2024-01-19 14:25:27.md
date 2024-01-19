```modula-2
MODULE Main;
IMPORT Out;

TYPE Word = ARRAY 32 OF BOOLEAN; -- Un tipo de datos para representar palabras de 32 bits
TYPE Memory = ARRAY 1000 OF Byte; -- Un tipo de datos para representar memoria con 1000 bytes

VAR memory: Memory; -- La memoria del ordenador
VAR accumulator: Word; -- El acumulador del ordenador
VAR programCounter: INTEGER; -- El contador de programa del ordenador
VAR instructionRegister: Word; -- El registro de instrucciones del ordenador

PROCEDURE IncrementAccumulator;
BEGIN
  accumulator := accumulator + 1; -- Incrementar el acumulador en 1
END IncrementAccumulator;

PROCEDURE DecrementAccumulator;
BEGIN
  accumulator := accumulator - 1; -- Decrementar el acumulador en 1
END DecrementAccumulator;

PROCEDURE Jump(address: INTEGER);
BEGIN
  programCounter := address; -- Saltar a la dirección especificada
END Jump;

PROCEDURE Load(address: INTEGER);
BEGIN
  accumulator := memory[address]; -- Cargar el valor de la memoria en el acumulador
END Load;

PROCEDURE Store(address: INTEGER);
BEGIN
  memory[address] := accumulator; -- Almacenar el valor del acumulador en la memoria
END Store;

PROCEDURE PrintAccumulator;
BEGIN
  Out.String("El acumulador contiene: ");
  Out.Word(accumulator, 10);
  Out.Ln; -- Imprimir el valor del acumulador
END PrintAccumulator;

PROCEDURE Run;
BEGIN
  -- Inicializar el ordenador
  accumulator := 0;
  programCounter := 0;

  -- Cargar el programa en la memoria
  memory[0] := 1; -- Incrementar el acumulador
  memory[1] := 2; -- Imprimir el acumulador
  memory[2] := 3; -- Salir del programa

  -- Ejecutar el programa
  WHILE TRUE DO
    instructionRegister := memory[programCounter]; -- Obtener la siguiente instrucción
    CASE instructionRegister OF
      1: IncrementAccumulator; -- Incrementar el acumulador
      2: PrintAccumulator; -- Imprimir el acumulador
      3: EXIT; -- Salir del programa
    END;
    programCounter := programCounter + 1; -- Incrementar el contador de programa
  END;
END Run;

BEGIN
  -- Ejecutar el programa
  Run;
END Main.
```

Este código es un programa completo en el lenguaje de programación Modula-2. El programa simula un ordenador simple con un acumulador, un contador de programa y memoria. El programa carga un programa en la memoria y luego lo ejecuta. El programa consta de tres instrucciones: incrementar el acumulador, imprimir el acumulador y salir del programa. El programa se ejecuta en un bucle hasta que la instrucción de salida se alcanza.

El programa utiliza los siguientes tipos de datos:

* **Word**: Un tipo de datos para representar palabras de 32 bits.
* **Memory**: Un tipo de datos para representar memoria con 1000 bytes.

El programa utiliza los siguientes procedimientos:

* **IncrementAccumulator**: Incrementar el acumulador en 1.
* **DecrementAccumulator**: Decrementar el acumulador en 1.
* **Jump**: Saltar a la dirección especificada.
* **Load**: Cargar el valor de la memoria en el acumulador.
* **Store**: Almacenar el valor del acumulador en la memoria.
* **PrintAccumulator**: Imprimir el valor del acumulador.
* **Run**: Ejecutar el programa.

El programa principal primero inicializa el ordenador y luego carga el programa en la memoria. El programa luego ejecuta el programa en un bucle hasta que la instrucción de salida se alcanza.

El programa es una implementación simple de un ordenador y puede utilizarse para demostrar los conceptos básicos de la programación de ordenadores.