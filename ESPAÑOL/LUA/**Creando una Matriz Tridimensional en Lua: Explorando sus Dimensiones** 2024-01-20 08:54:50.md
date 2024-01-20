```lua
-- Este código crea una matriz tridimensional de tamaño 3x3x3 y la llena con valores aleatorios.

-- Creamos una matriz tridimensional vacía.
local matriz = {}

-- Recorremos cada dimensión de la matriz y la llenamos con valores aleatorios.
for i = 1, 3 do
  matriz[i] = {}
  for j = 1, 3 do
    matriz[i][j] = {}
    for k = 1, 3 do
      matriz[i][j][k] = math.random(1, 100)
    end
  end
end

-- Imprimimos la matriz en la consola.
for i = 1, 3 do
  for j = 1, 3 do
    for k = 1, 3 do
      print(matriz[i][j][k])
    end
  end
end

```

Explicación:

* La primera línea crea una matriz tridimensional vacía.
* El siguiente bucle for recorre cada dimensión de la matriz y la llena con valores aleatorios.
* El segundo bucle for recorre la segunda dimensión de la matriz y la llena con valores aleatorios.
* El tercer bucle for recorre la tercera dimensión de la matriz y la llena con valores aleatorios.
* La función `math.random(1, 100)` genera un número aleatorio entre 1 y 100.
* El último bucle for imprime la matriz en la consola.