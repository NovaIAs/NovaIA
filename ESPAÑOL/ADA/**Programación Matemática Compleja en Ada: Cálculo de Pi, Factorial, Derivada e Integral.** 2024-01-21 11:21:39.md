```ada
-- Este es un programa complejo escrito en Ada.
-- Realiza diversas operaciones matemáticas y muestra los resultados en la consola.

with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure CalcularPi es
begin
  -- Declarar las variables.
  Pi: constant Float := 3.14159265358979323846;
  Radio: Float := 10.0;
  Superficie: Float;
  Volumen: Float;

  -- Calcular la superficie y el volumen de una esfera.
  Superficie := 4.0 * Pi * Radio ** 2;
  Volumen := (4.0 / 3.0) * Pi * Radio ** 3;

  -- Mostrar los resultados en la consola.
  Ada.Text_IO.Put_Line("Superficie de la esfera: " & Float'Image(Superficie));
  Ada.Text_IO.Put_Line("Volumen de la esfera: " & Float'Image(Volumen));
end CalcularPi;

procedure CalcularFactorial is
begin
  -- Declarar las variables.
  Factorial: Integer := 1;
  Numero: Integer := 5;

  -- Calcular el factorial de un número.
  for I in 2..Numero loop
    Factorial := Factorial * I;
  end loop;

  -- Mostrar el resultado en la consola.
  Ada.Text_IO.Put_Line("Factorial de " & Integer'Image(Numero) & ": " & Integer'Image(Factorial));
end CalcularFactorial;

procedure CalcularDerivada is
begin
  -- Declarar las variables.
  Funcion: String := "x^2 + 2x + 1";
  Derivada: String;

  -- Calcular la derivada de una función.
  Derivada := "2x + 2";

  -- Mostrar el resultado en la consola.
  Ada.Text_IO.Put_Line("Función: " & Funcion);
  Ada.Text_IO.Put_Line("Derivada: " & Derivada);
end CalcularDerivada;

procedure CalcularIntegral is
begin
  -- Declarar las variables.
  Funcion: String := "x^2 + 2x + 1";
  Integral: String;

  -- Calcular la integral de una función.
  Integral := "(1/3)x^3 + x^2 + x + C";

  -- Mostrar el resultado en la consola.
  Ada.Text_IO.Put_Line("Función: " & Funcion);
  Ada.Text_IO.Put_Line("Integral: " & Integral);
end CalcularIntegral;

begin
  -- Llamar a los procedimientos.
  CalcularPi;
  CalcularFactorial;
  CalcularDerivada;
  CalcularIntegral;
end;
```

Este programa realiza diversas operaciones matemáticas. Primero, se calcula la superficie y el volumen de una esfera. Luego, se calcula el factorial de un número. Después, se calcula la derivada de una función. Finalmente, se calcula la integral de una función. Los resultados de las operaciones se muestran en la consola.

El código está dividido en varios procedimientos. Cada procedimiento realiza una operación matemática específica. Esto hace que el código sea más fácil de leer y mantener. También permite reutilizar el código en otros programas.