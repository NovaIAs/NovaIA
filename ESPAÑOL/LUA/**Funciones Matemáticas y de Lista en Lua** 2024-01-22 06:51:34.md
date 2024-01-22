```lua
-- Creamos una función para saludar a una persona.
function saludar(nombre)
  -- Imprimimos un mensaje de saludo.
  print("¡Hola, " .. nombre .. "!")
end

-- Creamos una función para sumar dos números.
function sumar(numero1, numero2)
  -- Devolvemos la suma de los dos números.
  return numero1 + numero2
end

-- Creamos una función para restar dos números.
function restar(numero1, numero2)
  -- Devolvemos la resta de los dos números.
  return numero1 - numero2
end

-- Creamos una función para multiplicar dos números.
function multiplicar(numero1, numero2)
  -- Devolvemos la multiplicación de los dos números.
  return numero1 * numero2
end

-- Creamos una función para dividir dos números.
function dividir(numero1, numero2)
  -- Devolvemos la división de los dos números.
  return numero1 / numero2
end

-- Creamos una función para calcular el factorial de un número.
function factorial(numero)
  -- Si el número es igual a 0 o 1, devolvemos 1.
  if numero == 0 or numero == 1 then
    return 1
  end

  -- Devolvemos el factorial del número multiplicándolo por el factorial del número menos 1.
  return numero * factorial(numero - 1)
end

-- Creamos una función para calcular el máximo común divisor de dos números.
function maximoComunDivisor(numero1, numero2)
  -- Mientras el segundo número sea distinto de 0, intercambiamos los dos números y seguimos calculando el máximo común divisor.
  while numero2 ~= 0 do
    numero1, numero2 = numero2, numero1 % numero2
  end

  -- Devolvemos el máximo común divisor.
  return numero1
end

-- Creamos una función para calcular el mínimo común múltiplo de dos números.
function minimoComunMultiplo(numero1, numero2)
  -- Calculamos el máximo común divisor de los dos números.
  mcd = maximoComunDivisor(numero1, numero2)

  -- Devolvemos el mínimo común múltiplo de los dos números.
  return (numero1 * numero2) / mcd
end

-- Creamos una función para calcular la potencia de un número.
function potencia(numero, exponente)
  -- Si el exponente es igual a 0, devolvemos 1.
  if exponente == 0 then
    return 1
  end

  -- Devolvemos el número elevado a la potencia multiplicándolo por sí mismo el número de veces que indique el exponente.
  return numero * potencia(numero, exponente - 1)
end

-- Creamos una función para calcular la raíz cuadrada de un número.
function raizCuadrada(numero)
  -- Comprobamos si el número es negativo.
  if numero < 0 then
    -- Devolvemos un error.
    return "El número no puede ser negativo."
  end

  -- Calculamos la raíz cuadrada del número utilizando la función incorporada de Lua.
  return math.sqrt(numero)
end

-- Creamos una función para calcular la media de una lista de números.
function media(lista)
  -- Sumamos todos los números de la lista.
  suma = 0
  for i = 1, #lista do
    suma = suma + lista[i]
  end

  -- Dividimos la suma por el número de elementos de la lista y devolvemos la media.
  return suma / #lista
end

-- Creamos una función para calcular la desviación estándar de una lista de números.
function desviacionEstandar(lista)
  -- Calculamos la media de la lista.
  media = media(lista)

  -- Calculamos la suma de las diferencias al cuadrado entre cada número de la lista y la media.
  sumaCuadrados = 0
  for i = 1, #lista do
    sumaCuadrados = sumaCuadrados + (lista[i] - media)^2
  end

  -- Dividimos la suma de las diferencias al cuadrado por el número de elementos de la lista y calculamos la raíz cuadrada.
  return math.sqrt(sumaCuadrados / #lista)
end

-- Creamos una función para ordenar una lista de números en orden ascendente.
function ordenar(lista)
  -- Utilizamos la función incorporada de Lua para ordenar la lista.
  table.sort(lista)

  -- Devolvemos la lista ordenada.
  return lista
end

-- Creamos una función para buscar un elemento en una lista de números.
function buscar(lista, elemento)
  -- Recorremos la lista y comprobamos si el elemento está en ella.
  for i = 1, #lista do
    if lista[i] == elemento then
      -- Devolvemos la posición del elemento en la lista.
      return i
    end
  end

  -- Devolvemos -1 si el elemento no está en la lista.
  return -1
end

-- Creamos una función para eliminar un elemento de una lista de números.
function eliminar(lista, elemento)
  -- Recorremos la lista y comprobamos si el elemento está en ella.
  for i = 1, #lista do
    if lista[i] == elemento then
      -- Eliminamos el elemento de la lista.
      table.remove(lista, i)

      -- Devolvemos la lista sin el elemento.
      return lista
    end
  end

  -- Devolvemos la lista original si el elemento no está en ella.
  return lista
end

-- Creamos una función para añadir un elemento a una lista de números.
function añadir(lista, elemento)
  -- Añadimos el elemento a la lista.
  lista[#lista + 1] = elemento

  -- Devolvemos la lista con el elemento añadido.
  return lista
end

-- Creamos una función para imprimir una lista de números.
function imprimir(lista)
  -- Recorremos la lista y imprimimos cada elemento.
  for i = 1, #lista do
    print(lista[i])
  end
end

-- Creamos una lista de números.
lista = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

-- Imprimimos la lista original.
print("Lista original:")
imprimir(lista)

-- Añadimos un elemento a la lista.
lista = añadir(lista, 11)

-- Imprimimos la lista con el elemento añadido.
print("\nLista con el elemento añadido:")
imprimir(lista)

-- Eliminamos un elemento de la lista.
lista = eliminar(lista, 5)

-- Imprimimos la lista con el elemento eliminado.
print("\nLista con el elemento eliminado:")
imprimir(lista)

-- Buscamos un elemento en la lista.
posicion = buscar(lista, 7)

-- Imprimimos la posición del elemento en la lista.
print("\nPosición del elemento 7 en la lista:")
print(posicion)

-- Ordenamos la lista.
lista = ordenar(lista)

-- Imprimimos la lista ordenada.
print("\nLista ordenada:")
imprimir(lista)

-- Calculamos la media de la lista.
media = media(lista)

-- Imprimimos la media de la lista.
print("\nMedia de la lista:")
print(media)

-- Calculamos la desviación estándar de la lista.
desviacionEstandar = desviacionEstandar(lista)

-- Imprimimos la desviación estándar de la lista.
print("\nDesviación estándar de la lista:")
print(desviacionEstandar)
```

Explicación del código:

* Creamos una serie de funciones para realizar operaciones matemáticas básicas, como sumar, restar, multiplicar y dividir.
* También creamos funciones para calcular el factorial de un número, el máximo común divisor de dos números, el mínimo común múltiplo de dos números, la potencia de un número y la raíz cuadrada de un número.
* Creamos una función para calcular la media y la desviación estándar de una lista de números.
* También creamos funciones para ordenar una lista de números, buscar un elemento en una lista de números, eliminar un elemento de una lista de números y añadir un elemento a una lista de números.
* Finalmente, creamos una función para imprimir una lista de números.
* Creamos una lista de números y la imprimimos.
* Añadimos un elemento a la lista y la imprimimos.
* Eliminamos un elemento de la lista y la imprimimos.
* Buscamos un elemento en la lista y imprimimos su posición.
* Ordenamos la lista y la imprimimos.
* Calculamos la media y la desviación estándar de la lista y las imprimimos.