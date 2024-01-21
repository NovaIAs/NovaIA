```elixir
# Función para sumar dos números
def sumar(a, b) do
  a + b
end

# Función para restar dos números
def restar(a, b) do
  a - b
end

# Función para multiplicar dos números
def multiplicar(a, b) do
  a * b
end

# Función para dividir dos números
def dividir(a, b) do
  a / b
end

# Función para calcular el factorial de un número
def factorial(n) do
  if n == 0 do
    1
  else
    n * factorial(n-1)
  end
end

# Función para calcular el máximo común divisor de dos números
def mcd(a, b) do
  if b == 0 do
    a
  else
    mcd(b, a rem b)
  end
end

# Función para calcular el mínimo común múltiplo de dos números
def mcm(a, b) do
  a * b / mcd(a, b)
end

# Función para calcular la suma de los primeros n números naturales
def suma_naturales(n) do
  if n == 0 do
    0
  else
    n + suma_naturales(n-1)
  end
end

# Función para calcular el producto de los primeros n números naturales
def producto_naturales(n) do
  if n == 0 do
    1
  else
    n * producto_naturales(n-1)
  end
end

# Función para calcular la potencia de un número elevado a otro número
def potencia(a, b) do
  if b == 0 do
    1
  else
    a * potencia(a, b-1)
  end
end

# Función para calcular la raíz cuadrada de un número
def raiz_cuadrada(n) do
  Math.sqrt(n)
end

# Función para calcular el seno de un ángulo
def seno(angulo) do
  Math.sin(angulo)
end

# Función para calcular el coseno de un ángulo
def coseno(angulo) do
  Math.cos(angulo)
end

# Función para calcular la tangente de un ángulo
def tangente(angulo) do
  Math.tan(angulo)
end

# Función para calcular el arco seno de un ángulo
def arco_seno(angulo) do
  Math.asin(angulo)
end

# Función para calcular el arco coseno de un ángulo
def arco_coseno(angulo) do
  Math.acos(angulo)
end

# Función para calcular el arco tangente de un ángulo
def arco_tangente(angulo) do
  Math.atan(angulo)
end
```

Este código es una colección de funciones matemáticas comunes escritas en Elixir. Incluye funciones para realizar operaciones básicas como sumar, restar, multiplicar y dividir, así como funciones para calcular funciones más avanzadas como el factorial, el máximo común divisor y el mínimo común múltiplo. También incluye funciones para calcular la suma y el producto de los primeros n números naturales, así como la potencia de un número elevado a otro número.

Además, incluye funciones para calcular la raíz cuadrada, el seno, el coseno y la tangente de un ángulo, así como las funciones inversas arco seno, arco coseno y arco tangente.

Este código es útil para realizar cálculos matemáticos comunes en Elixir, y puede ser utilizado en una variedad de aplicaciones.