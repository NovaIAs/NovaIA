```lua
-- Este código crea una matriz de 10x10 y la llena con números aleatorios.

-- Crear una matriz de 10x10
matriz = {}
for i = 1, 10 do
  matriz[i] = {}
  for j = 1, 10 do
    matriz[i][j] = math.random(1, 100)
  end
end

-- Mostrar la matriz en la consola
print("Matriz:")
for i = 1, 10 do
  for j = 1, 10 do
    print(matriz[i][j], "\t")
  end
  print()
end

-- Encontrar el mayor y el menor número de la matriz
mayor = 0
menor = 101
for i = 1, 10 do
  for j = 1, 10 do
    if matriz[i][j] > mayor then
      mayor = matriz[i][j]
    end
    if matriz[i][j] < menor then
      menor = matriz[i][j]
    end
  end
end

-- Mostrar el mayor y el menor número en la consola
print("Mayor número:", mayor)
print("Menor número:", menor)

-- Calcular la suma y el promedio de los números de la matriz
suma = 0
for i = 1, 10 do
  for j = 1, 10 do
    suma = suma + matriz[i][j]
  end
end
promedio = suma / (10 * 10)

-- Mostrar la suma y el promedio en la consola
print("Suma:", suma)
print("Promedio:", promedio)
```

Este código crea una matriz de 10x10 y la llena con números aleatorios entre 1 y 100. Luego, muestra la matriz en la consola, encuentra el mayor y el menor número de la matriz, calcula la suma y el promedio de los números de la matriz, y muestra estos valores en la consola.