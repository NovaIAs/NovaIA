```ruby
# Programa que simula el lanzamiento de dos dados y devuelve la suma de sus caras.

# Definición de métodos

# Método para generar un número aleatorio entre 1 y 6.
def tirar_dado
  rand(1..6)
end

# Método para obtener la suma de las caras de dos dados.
def sumar_dados(dado1, dado2)
  dado1 + dado2
end

# Método para imprimir el resultado del lanzamiento de los dados.
def imprimir_resultado(resultado)
  puts "La suma de las caras de los dados es #{resultado}"
end

# Programa principal

# Generamos dos números aleatorios entre 1 y 6.
dado1 = tirar_dado
dado2 = tirar_dado

# Obtenemos la suma de las caras de los dados.
resultado = sumar_dados(dado1, dado2)

# Imprimimos el resultado del lanzamiento de los dados.
imprimir_resultado(resultado)
```

Explicación del código:

* El código define tres métodos: `tirar_dado`, `sumar_dados` e `imprimir_resultado`.
* El método `tirar_dado` genera un número aleatorio entre 1 y 6 utilizando el método `rand`.
* El método `sumar_dados` recibe dos números como parámetros y devuelve su suma.
* El método `imprimir_resultado` recibe un número como parámetro y lo imprime por la consola.
* El programa principal genera dos números aleatorios entre 1 y 6, los suma y luego imprime el resultado.