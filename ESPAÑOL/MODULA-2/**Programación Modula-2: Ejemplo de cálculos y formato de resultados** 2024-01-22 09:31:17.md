```modula-2
MODULE Math;
FROM FormatString IMPORT FormatString, FormatValue;
FROM FormatNumber IMPORT FormatFloat;

PROCEDURE Demostracion(x: REAL; y: REAL; z: REAL):
VAR s: STRING;
BEGIN
  FormatString('Resultados', s);
  FormatValue(s, x, 50);
  FormatValue(s, y, 50);
  FormatValue(s, z, 50);
  FormatValue(s, (x+y+z), 50);
  FormatValue(s, (x*y*z), 50);
  FormatValue(s, (x/y/z), 50);
  FormatValue(s, (x-y-z), 50);
END Demostracion;

PROCEDURE Main:

VAR x, y, z: REAL;
BEGIN
  x := 10.0;
  y := 20.0;
  z := 30.0;
  Demostracion(x, y, z);
END Main.
```

Explicación:

* El módulo `Math` define un procedimiento llamado `Demostracion` que toma tres parámetros reales `x`, `y` y `z` y realiza una serie de cálculos sobre ellos. Los resultados se formatean y muestran en una cadena.
* El procedimiento `Main` llama al procedimiento `Demostracion` con los valores `x = 10.0`, `y = 20.0` y `z = 30.0`.
* El procedimiento `FormatString` se utiliza para formatear una cadena.
* El procedimiento `FormatValue` se utiliza para formatear un valor en una cadena.
* El procedimiento `FormatFloat` se utiliza para formatear un valor de coma flotante en una cadena.