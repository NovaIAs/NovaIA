```modula-2
MODULE LenguajeDeProgramacion;

IMPORT IO;

VAR
  entero, real: INTEGER;
  cadena: TEXT;

BEGIN
  IO.Write("Ingrese un entero: ");
  entero := IO.ReadInteger;
  IO.Write("Ingrese un real: ");
  real := IO.ReadReal;
  IO.Write("Ingrese una cadena: ");
  IO.ReadText(cadena);

  IF entero MOD 2 = 0 THEN
    IO.Write("El entero es par.\n");
  ELSE
    IO.Write("El entero es impar.\n");
  END;

  IF real > 0 THEN
    IO.Write("El real es positivo.\n");
  ELSIF real < 0 THEN
    IO.Write("El real es negativo.\n");
  ELSE
    IO.Write("El real es cero.\n");
  END;

  IO.Write("La longitud de la cadena es: ", IO.Length(cadena));
  IF IO.Length(cadena) > 0 THEN
    IO.Write(" y la cadena es: ", cadena);
  END;
  IO.WriteLn;
END LenguajeDeProgramacion.
```

Este código en MODULA-2 es un programa que solicita al usuario que ingrese un entero, un real y una cadena y luego imprime información sobre ellos.

- `VAR` declara las variables que se utilizarán en el programa.
- `BEGIN` y `END` son las palabras clave que delimitan el cuerpo del programa.
- `IO.Write` imprime un mensaje en la consola.
- `IO.ReadInteger` lee un entero ingresado por el usuario.
- `IO.ReadReal` lee un real ingresado por el usuario.
- `IO.ReadText` lee una cadena ingresada por el usuario.
- `IF` y `ELSE` se utilizan para crear una instrucción condicional.
- `MOD` es el operador módulo que devuelve el resto de una división entera.
- `Length` devuelve la longitud de una cadena.
- `ELSIF` es una forma abreviada de escribir `ELSE IF`.
- `IO.WriteLn` imprime una nueva línea en la consola.