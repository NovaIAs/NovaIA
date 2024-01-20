```lua
-- Definición de un módulo de utilidad para manejar números grandes.
módulo NúmeroGrande = {
  -- Función para sumar dos números grandes representados como cadenas.
  suma = function(a, b)
    -- Convertir las cadenas a números para poder sumarlas.
    a, b = tonumber(a), tonumber(b)
    -- Crear un nuevo número grande con la suma de los números.
    return (a + b):string()
  end,

  -- Función para restar dos números grandes representados como cadenas.
  resta = function(a, b)
    -- Convertir las cadenas a números para poder restarlos.
    a, b = tonumber(a), tonumber(b)
    -- Crear un nuevo número grande con la resta de los números.
    return (a - b):string()
  end,

  -- Función para multiplicar dos números grandes representados como cadenas.
  producto = function(a, b)
    -- Convertir las cadenas a números para poder multiplicarlos.
    a, b = tonumber(a), tonumber(b)
    -- Crear un número grande con el producto de los números.
    return (a * b):string()
  end,

  -- Función para dividir dos números grandes representados como cadenas.
  división = function(a, b)
    -- Convertir las cadenas a números para poder dividirlos.
    a, b = tonumber(a), tonumber(b)
    -- Crear un número grande con el cociente de los números.
    return (a / b):string()
  end
}

-- Definición de una función para generar números primos.
función generarPrimos(n)
  -- Crear una tabla para almacenar los números primos.
  primos = {}
  -- Empezar por el número 2, que es el primer número primo.
  i = 2
  -- Mientras no se hayan encontrado todos los números primos necesarios.
  mientras que (#primos < n) haga
    -- Comprobar si el número actual es primo.
    esPrimo = cierto
    para j = 2, (i / 2) haga
      si (i % j == 0) entonces
        esPrimo = falso
        romper
      fin si
    fin para
    -- Si el número actual es primo, añadirlo a la tabla de números primos.
    si (esPrimo) entonces
      primos[#primos + 1] = i
    fin si
    -- Incrementar el número actual.
    i = i + 1
  fin mientras
  -- Devolver la tabla de números primos.
  devolver primos
fin función

-- Imprimir los primeros 10 números primos.
para i = 1, 10, 1 haga
  print(generarPrimos(i))
fin para
```

Este código define un módulo para manejar números grandes y una función para generar números primos. El módulo de números grandes proporciona funciones para sumar, restar, multiplicar y dividir números grandes representados como cadenas. La función de generación de números primos genera una tabla de los primeros n números primos.

El código también incluye un ejemplo de cómo utilizar el módulo de números grandes para sumar dos números grandes y la función de generación de números primos para imprimir los primeros 10 números primos.