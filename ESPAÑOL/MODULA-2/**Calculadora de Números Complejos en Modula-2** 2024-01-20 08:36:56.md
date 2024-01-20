```modula-2
MODULE Complejo;
FROM FormatString IMPORT Format, FormatI, FormatR;
FROM Terminal IMPORT WriteString, WriteCard, WriteLn;
FROM RealNumbers IMPORT FLOAT;

PROCEDURE Complejo(a, b: FLOAT): FLOAT;
BEGIN
  IF a > 0 THEN
    RETURN a
  ELSE
    RETURN b
  END
END Complejo;

PROCEDURE Main;
VAR a, b: ARRAY [0..2] OF FLOAT;
    i: CARDINAL;
BEGIN
  FOR i := 0 TO 2 DO
    BEGIN
      WriteString("Ingrese los valores reales e imaginarios del número complejo ");
      WriteCard(i);
      WriteString(": ");
      Read(a[i], b[i])
    END;
  WriteString("Los números complejos ingresados son: ");
  FOR i := 0 TO 2 DO
    BEGIN
      FormatR(R, 5, 2, Complejo(a[i], b[i]));
      WriteString(" (");
      FormatI(I, 5, 0, a[i]);
      WriteString(" + ");
      FormatR(R, 5, 2, b[i]);
      WriteString("j) ");
    END;
  WriteLn
END Main.
```

Explicación del código:

* El módulo `Complejo` define una función `Complejo` que toma dos argumentos reales, `a` y `b`, y devuelve el número complejo resultante.
* El procedimiento `Main` es el punto de entrada principal del programa.
* El procedimiento `Main` primero declara tres variables de tipo `FLOAT`: `a`, `b` e `i`.
* El procedimiento `Main` utiliza un ciclo `FOR` para solicitar al usuario que ingrese los valores reales e imaginarios de tres números complejos.
* El procedimiento `Main` utiliza el procedimiento `Complejo` para calcular los números complejos resultantes y los almacena en la variable `c`.
* El procedimiento `Main` utiliza un ciclo `FOR` para mostrar los números complejos resultantes en la consola.
* El procedimiento `Main` utiliza los procedimientos `FormatR` y `FormatI` para formatear los números complejos en una cadena de caracteres legible.