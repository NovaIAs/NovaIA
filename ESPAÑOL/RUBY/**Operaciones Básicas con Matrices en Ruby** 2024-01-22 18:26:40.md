```ruby
# Crear una matriz de números del 1 al 10
matriz = Array.new(10) { |i| i + 1 }

# Definir un procedimiento para imprimir los elementos de la matriz
def imprimir_matriz(matriz)
  matriz.each do |elemento|
    puts elemento
  end
end

# Llamar al procedimiento para imprimir la matriz
imprimir_matriz(matriz)

# Definir un procedimiento para calcular la suma de los elementos de la matriz
def sumar_matriz(matriz)
  suma = 0
  matriz.each do |elemento|
    suma += elemento
  end
  suma
end

# Llamar al procedimiento para calcular la suma de la matriz
suma = sumar_matriz(matriz)

# Imprimir la suma de la matriz
puts "La suma de los elementos de la matriz es #{suma}"

# Definir un procedimiento para ordenar los elementos de la matriz de forma ascendente
def ordenar_matriz(matriz)
  matriz.sort
end

# Llamar al procedimiento para ordenar la matriz
matriz_ordenada = ordenar_matriz(matriz)

# Imprimir la matriz ordenada
imprimir_matriz(matriz_ordenada)

# Definir un procedimiento para buscar un elemento en la matriz usando búsqueda binaria
def buscar_binaria(matriz, elemento)
  izquierda = 0
  derecha = matriz.size - 1

  while izquierda <= derecha
    medio = (izquierda + derecha) / 2
    if matriz[medio] == elemento
      return medio
    elsif matriz[medio] < elemento
      izquierda = medio + 1
    else
      derecha = medio - 1
    end
  end

  -1
end

# Llamar al procedimiento para buscar el elemento 5 en la matriz
indice = buscar_binaria(matriz, 5)

# Imprimir el índice del elemento encontrado, o -1 si no se encontró
puts "El elemento 5 se encuentra en el índice #{indice}"
```

Este código crea una matriz de números del 1 al 10, llama a un procedimiento para imprimir los elementos de la matriz, calcula la suma de los elementos de la matriz, llama a un procedimiento para ordenar los elementos de la matriz de forma ascendente, llama a un procedimiento para buscar un elemento en la matriz usando búsqueda binaria, y llama a un procedimiento para imprimir el índice del elemento encontrado, o -1 si no se encontró.