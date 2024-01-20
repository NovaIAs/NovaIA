```lua
-- Función para generar una lista de números primos hasta un límite dado
function generar_primos(limite)
  -- Crear una tabla para almacenar los números primos
  numeros_primos = {}

  -- Inicializar el primer número primo en 2
  numero_primo = 2

  -- Iterar hasta el límite dado
  while numero_primo <= limite do
    -- Comprobar si el número es primo
    es_primo = true
    for i = 2, math.sqrt(numero_primo) do
      if numero_primo % i == 0 then
        es_primo = false
        break
      end
    end

    -- Si el número es primo, agregarlo a la tabla
    if es_primo then
      table.insert(numeros_primos, numero_primo)
    end

    -- Incrementar el número primo para el siguiente paso
    numero_primo = numero_primo + 1
  end

  -- Devolver la tabla de números primos
  return numeros_primos
end

-- Función para encontrar el factor primo más grande de un número dado
function factor_primo_mas_grande(numero)
  -- Obtener la lista de números primos hasta la raíz cuadrada del número
  numeros_primos = generar_primos(math.sqrt(numero))

  -- Iterar sobre los números primos
  for i = 1, #numeros_primos do
    numero_primo = numeros_primos[i]

    -- Comprobar si el número primo divide al número
    if numero % numero_primo == 0 then
      -- Si el número primo divide al número, devolverlo
      return numero_primo
    end
  end

  -- Si no se encuentra ningún factor primo, devolver el propio número
  return numero
end

-- Función para descomponer un número en sus factores primos
function descomponer_en_primos(numero)
  -- Crear una tabla para almacenar los factores primos
  factores_primos = {}

  -- Obtener el factor primo más grande del número
  factor_primo = factor_primo_mas_grande(numero)

  -- Mientras el factor primo sea mayor que 1, seguir descomponiendo el número
  while factor_primo > 1 do
    -- Agregar el factor primo a la tabla
    table.insert(factores_primos, factor_primo)

    -- Dividir el número entre el factor primo para obtener el siguiente número a descomponer
    numero = numero / factor_primo

    -- Obtener el factor primo más grande del nuevo número
    factor_primo = factor_primo_mas_grande(numero)
  end

  -- Devolver la tabla de factores primos
  return factores_primos
end

-- Ejemplo de uso
-- Obtener la lista de números primos hasta 100
numeros_primos = generar_primos(100)
print("Números primos hasta 100:")
for i = 1, #numeros_primos do
  print(numeros_primos[i])
end

-- Obtener el factor primo más grande de 123456
factor_primo_mas_grande_123456 = factor_primo_mas_grande(123456)
print("Factor primo más grande de 123456:")
print(factor_primo_mas_grande_123456)

-- Descomponer 123456 en sus factores primos
factores_primos_123456 = descomponer_en_primos(123456)
print("Factores primos de 123456:")
for i = 1, #factores_primos_123456 do
  print(factores_primos_123456[i])
end
```

Explicación del código:

* La función `generar_primos` genera una tabla de números primos hasta un límite dado. Utiliza un algoritmo conocido como criba de Eratóstenes para encontrar los números primos de manera eficiente.
* La función `factor_primo_mas_grande` encuentra el factor primo más grande de un número dado. Utiliza una lista de números primos generada previamente para comprobar si el número es divisible por alguno de estos números primos.
* La función `descomponer_en_primos` descompone un número en sus factores primos utilizando la función `factor_primo_mas_grande`. Divide el número repetidamente por su factor primo más grande hasta que el número sea primo.

El ejemplo de uso muestra cómo utilizar estas funciones para generar la lista de números primos hasta 100, encontrar el factor primo más grande de 123456 y descomponer 123456 en sus factores primos.