```modula-2

MODULE Complejo;
FROM Text IMPORT WriteCard, WriteString, WriteLn;

VAR
  Error: STRING(7) := /* vacío */;

PROCEDURE Raiz_Cuadrada(x: LONGCARD): LONGCARD;
VAR
  y: LONGCARD;
BEGIN
  y := 0;
  WHILE y * y < x DO
    y := y + 1
  END;
  IF y * y <> x THEN
    Error := "Inexacto";
    y := 0
  END
END Raiz_Cuadrada;

PROCEDURE Divisores_Primos(n: LONGCARD; VAR d: ARRAY OF LONGCARD);
VAR
  i, j, cont: LONGCARD;
BEGIN
  d := [];
  j := 0;
  cont := 0;
  WHILE n > 1 DO
    i := 2;
    WHILE n MOD i <> 0 DO
      i := i + 1
    END;
    n := n DIV i;
    j := j + 1;
    IF d[j] <> i THEN
      d[j] := i;
      cont := cont + 1
    END
  END;
  d := d[0 .. cont-1]
END Divisores_Primos;

PROCEDURE Primos_Menores(n: LONGCARD; VAR p: ARRAY OF LONGCARD);
VAR
  i, j, k, cont: LONGCARD;
BEGIN
  p := [];
  j := 0;
  cont := 0;
  FOR i FROM 2 TO n DO
    k := 0;
    WHILE j < cont DO
      IF p[j] <= Raiz_Cuadrada(i) THEN
        IF i MOD p[j] = 0 THEN
          k := k + 1;
          EXIT
        END
      END;
      j := j + 1
    END;
    IF k = 0 THEN
      cont := cont + 1;
      p[cont] := i
    END;
    j := 0
  END;
  p := p[0 .. cont-1]
END Primos_Menores;

BEGIN
  WriteString("Raíz cuadrada de 9: ");
  WriteCard(Raiz_Cuadrada(9));
  WriteLn();

  WriteString("Divisores primos de 12: ");
  VAR d: ARRAY OF LONGCARD;
  Divisores_Primos(12, d);
  FOR i FROM 0 TO HIGH(d) DO
    WriteCard(d[i]);
    IF i < HIGH(d) THEN
      WriteString(", ")
    END
  END;
  WriteLn();

  WriteString("Primos menores que 20: ");
  VAR p: ARRAY OF LONGCARD;
  Primos_Menores(20, p);
  FOR i FROM 0 TO HIGH(p) DO
    WriteCard(p[i]);
    IF i < HIGH(p) THEN
      WriteString(", ")
    END
  END;
  WriteLn();

  IF Error <> /* vacío */ THEN
    WriteLn(Error)
  END
END Complejo.

```

Explicación:

El código anterior es un programa en Modula-2 que realiza tres tareas:

1. Calcula la raíz cuadrada de un número.
2. Calcula los divisores primos de un número.
3. Calcula los números primos menores que un número.

El programa utiliza tres procedimientos:

1. `Raiz_Cuadrada`: Calcula la raíz cuadrada de un número utilizando el método de aproximación sucesiva.
2. `Divisores_Primos`: Calcula los divisores primos de un número utilizando el método de prueba de divisores.
3. `Primos_Menores`: Calcula los números primos menores que un número utilizando el método de la criba de Eratóstenes.

El programa principal utiliza los tres procedimientos para calcular la raíz cuadrada de 9, los divisores primos de 12 y los números primos menores que 20. Los resultados se imprimen en la consola.