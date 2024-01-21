```modula-2
MODULE Gráficos;
FROM Terminal IMPORT PonerColor, EscribirCadena;

VAR
  X, Y, Radio: INTEGER;

PROCEDURE DibujarCirculo(X, Y, Radio: INTEGER);
VAR
  i: INTEGER;
BEGIN
  FOR i := 0 TO 360 DO
    PonerColor(2, 15);
    EscribirCadena(X + Radio * COS(i * 3.141593 / 180),
                 Y + Radio * SIN(i * 3.141593 / 180),
                 "*");
  END;
END DibujarCirculo;

BEGIN
  X := 0;
  Y := 0;
  Radio := 5;
  DibujarCirculo(X, Y, Radio);
END Gráficos.
```

Este código dibuja un círculo en la pantalla utilizando la biblioteca Terminal para controlar el cursor y el color.

La función DibujarCirculo recibe tres parámetros: X e Y que son las coordenadas del centro del círculo, y Radio que es el radio del círculo.

Dentro de la función, hay un bucle FOR que recorre desde 0 hasta 360 grados. Se llama a la función PonerColor con el color 2 (rojo) y el color de fondo 15 (blanco).

La función EscribirCadena se llama con las coordenadas X e Y del píxel actual y el carácter "*". Esto dibuja un píxel rojo en la pantalla.

El bucle FOR se repite 360 veces, lo que dibuja un círculo completo.

La función Gráficos llama a la función DibujarCirculo para dibujar un círculo en el centro de la pantalla.