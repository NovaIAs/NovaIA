```lua
-- Este código es un programa que calcula el número de Fibonacci de un número dado.
-- El número de Fibonacci de un número n es el resultado de sumar los dos números anteriores en la serie.
-- Por ejemplo, los primeros números de Fibonacci son 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, etc.

-- Definir la función fibonacci que calcula el número de Fibonacci de un número dado.
function fibonacci(n)
  -- Si el número es 0 o 1, entonces su número de Fibonacci es igual al número mismo.
  if n == 0 or n == 1 then
    return n
  end

  -- Si el número es mayor que 1, entonces su número de Fibonacci es igual a la suma de los dos números anteriores en la serie.
  return fibonacci(n - 1) + fibonacci(n - 2)
end

-- Pedir al usuario que ingrese un número.
print("Ingrese un número:")
n = tonumber(io.read())

-- Calcular el número de Fibonacci del número ingresado por el usuario.
fib = fibonacci(n)

-- Imprimir el resultado.
print("El número de Fibonacci de", n, "es", fib)
```

Explicación del código:

* La función `fibonacci` calcula el número de Fibonacci de un número dado.
* Si el número es 0 o 1, entonces su número de Fibonacci es igual al número mismo.
* Si el número es mayor que 1, entonces su número de Fibonacci es igual a la suma de los dos números anteriores en la serie.
* La función `tonumber` convierte una cadena de caracteres en un número.
* La función `io.read` lee una línea de texto de la consola.
* La función `print` imprime un mensaje en la consola.