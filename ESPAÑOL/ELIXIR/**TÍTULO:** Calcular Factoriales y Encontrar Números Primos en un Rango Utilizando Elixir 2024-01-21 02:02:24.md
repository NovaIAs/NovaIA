```elixir
# Definir una función para calcular el factorial de un número

def factorial(numero) do
  if numero == 0, do: 1, else: numero * factorial(numero - 1)
end

# Definir una función para comprobar si un número es primo

def es_primo?(numero) do
  cond do
    numero <= 1 -> false
    numero == 2 -> true
    rem = rem(numero, 2) == 0 -> false
    true -> es_primo_recursivo(numero, 3)
  end
end

# Función recursiva para comprobar si un número es primo

def es_primo_recursivo(numero, divisor) do
  if divisor * divisor > numero, do: true, else:
    cond do
      rem(numero, divisor) == 0 -> false
      true -> es_primo_recursivo(numero, divisor + 2)
    end
end

# Definir una función para encontrar los números primos en un rango

def encontrar_primos(rango) do
  for numero <- rango, es_primo?(numero), do: numero
end

# Obtener el rango de números del usuario

IO.puts "Ingrese el rango inicial:"
rango_inicial = IO.gets("") |> String.to_integer()

IO.puts "Ingrese el rango final:"
rango_final = IO.gets("") |> String.to_integer()

# Encontrar los números primos en el rango

primos = encontrar_primos(rango_inicial..rango_final)

# Mostrar los números primos encontrados

IO.puts "Números primos en el rango #{rango_inicial} a #{rango_final}:"
IO.inspect primos
```

**Explicación del código:**

1. La función `factorial` calcula el factorial de un número utilizando recursividad. Si el número es 0, devuelve 1. De lo contrario, multiplica el número por el factorial del número anterior.

2. La función `es_primo?` comprueba si un número es primo. Primero, comprueba si el número es menor o igual que 1. Si es así, devuelve falso. Luego, comprueba si el número es 2. Si es así, devuelve verdadero. A continuación, comprueba si el número es par. Si es así, devuelve falso. Por último, utiliza una función recursiva (`es_primo_recursivo`) para comprobar si el número es primo.

3. La función `es_primo_recursivo` es una función recursiva que comprueba si un número es primo. Primero, comprueba si el divisor multiplicado por sí mismo es mayor que el número. Si es así, devuelve verdadero. De lo contrario, comprueba si el resto de dividir el número entre el divisor es 0. Si es así, devuelve falso. De lo contrario, llama a sí mismo con el número y el divisor más 2.

4. La función `encontrar_primos` encuentra los números primos en un rango. Utiliza un bucle for para iterar sobre el rango y comprueba si cada número es primo llamando a la función `es_primo?`. Si un número es primo, lo añade a una lista.

5. El código pide al usuario que introduzca el rango inicial y final.

6. Llama a la función `encontrar_primos` con el rango proporcionado por el usuario.

7. Muestra los números primos encontrados utilizando la función `IO.inspect`.