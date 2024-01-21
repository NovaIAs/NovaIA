```lua
-- Definir la función "promedio" que calcula el promedio de una lista de números.
function promedio(numeros)
  -- Comprobar si la lista de números está vacía.
  if #numeros == 0 then
    -- Si la lista está vacía, devolver 0.
    return 0
  end

  -- Calcular la suma de los números.
  local suma = 0
  for i = 1, #numeros do
    suma = suma + numeros[i]
  end

  -- Calcular el promedio.
  local promedio = suma / #numeros

  -- Devolver el promedio.
  return promedio
end

-- Definir la función "desviación_estándar" que calcula la desviación estándar de una lista de números.
function desviación_estándar(numeros)
  -- Comprobar si la lista de números está vacía.
  if #numeros == 0 then
    -- Si la lista está vacía, devolver 0.
    return 0
  end

  -- Calcular el promedio de los números.
  local promedio = promedio(numeros)

  -- Calcular la suma de las diferencias al cuadrado entre cada número y el promedio.
  local suma_diferencias_al_cuadrado = 0
  for i = 1, #numeros do
    suma_diferencias_al_cuadrado = suma_diferencias_al_cuadrado + (numeros[i] - promedio)^2
  end

  -- Calcular la varianza.
  local varianza = suma_diferencias_al_cuadrado / (#numeros - 1)

  -- Calcular la desviación estándar.
  local desviación_estándar = math.sqrt(varianza)

  -- Devolver la desviación estándar.
  return desviación_estándar
end

-- Definir la función "mediana" que calcula la mediana de una lista de números.
function mediana(numeros)
  -- Ordenar la lista de números en orden ascendente.
  table.sort(numeros)

  -- Comprobar si la lista tiene un número impar de elementos.
  if #numeros % 2 == 1 then
    -- Si la lista tiene un número impar de elementos, devolver el elemento del medio.
    return numeros[math.floor(#numeros / 2) + 1]
  else
    -- Si la lista tiene un número par de elementos, devolver el promedio de los dos elementos del medio.
    return (numeros[math.floor(#numeros / 2)] + numeros[math.floor(#numeros / 2) + 1]) / 2
  end
end

-- Definir la función "moda" que calcula la moda de una lista de números.
function moda(numeros)
  -- Crear una tabla para almacenar la frecuencia de cada número.
  local frecuencia = {}

  -- Contar el número de veces que aparece cada número en la lista.
  for i = 1, #numeros do
    local numero = numeros[i]
    if frecuencia[numero] then
      frecuencia[numero] = frecuencia[numero] + 1
    else
      frecuencia[numero] = 1
    end
  end

  -- Encontrar el número con la mayor frecuencia.
  local moda = nil
  local frecuencia_maxima = 0
  for numero, frecuencia in pairs(frecuencia) do
    if frecuencia > frecuencia_maxima then
      moda = numero
      frecuencia_maxima = frecuencia
    end
  end

  -- Devolver la moda.
  return moda
end

-- Definir la función "rango" que calcula el rango de una lista de números.
function rango(numeros)
  -- Encontrar el número máximo y el número mínimo en la lista.
  local maximo = numeros[1]
  local minimo = numeros[1]
  for i = 2, #numeros do
    if numeros[i] > maximo then
      maximo = numeros[i]
    elseif numeros[i] < minimo then
      minimo = numeros[i]
    end
  end

  -- Calcular el rango.
  local rango = maximo - minimo

  -- Devolver el rango.
  return rango
end

-- Definir la función "varianza" que calcula la varianza de una lista de números.
function varianza(numeros)
  -- Calcular el promedio de los números.
  local promedio = promedio(numeros)

  -- Calcular la suma de las diferencias al cuadrado entre cada número y el promedio.
  local suma_diferencias_al_cuadrado = 0
  for i = 1, #numeros do
    suma_diferencias_al_cuadrado = suma_diferencias_al_cuadrado + (numeros[i] - promedio)^2
  end

  -- Calcular la varianza.
  local varianza = suma_diferencias_al_cuadrado / (#numeros - 1)

  -- Devolver la varianza.
  return varianza
end

-- Definir la función "covarianza" que calcula la covarianza entre dos listas de números.
function covarianza(numeros1, numeros2)
  -- Comprobar si las dos listas tienen el mismo número de elementos.
  if #numeros1 ~= #numeros2 then
    -- Si las dos listas no tienen el mismo número de elementos, devolver 0.
    return 0
  end

  -- Calcular el promedio de cada lista.
  local promedio1 = promedio(numeros1)
  local promedio2 = promedio(numeros2)

  -- Calcular la suma de las diferencias entre cada número y el promedio de