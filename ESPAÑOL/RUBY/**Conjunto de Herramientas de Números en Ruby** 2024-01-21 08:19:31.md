```
def par_impar(número)
  # Verifica si el número es par o impar.
  if número % 2 == 0
    "El número #{número} es par."
  else
    "El número #{número} es impar."
  end
end

def factorial(número)
  # Calcula el factorial del número.
  if número == 0
    1
  else
    número * factorial(número - 1)
  end
end

def máximo_común_divisor(número_1, número_2)
  # Calcula el máximo común divisor de los dos números.
  if número_2 == 0
    número_1
  else
    máximo_común_divisor(número_2, número_1 % número_2)
  end
end

def menor_común_múltiplo(número_1, número_2)
  # Calcula el menor común múltiplo de los dos números.
  (número_1 * número_2) / máximo_común_divisor(número_1, número_2)
end

# Ejemplo de uso.
puts par_impar(10)
# Salida: "El número 10 es par."

puts factorial(5)
# Salida: "120"

puts máximo_común_divisor(21, 14)
# Salida: "7"

puts menor_común_múltiplo(21, 14)
# Salida: "42"
```

Explicación del código:

* La función `par_impar` verifica si el número dado es par o impar y devuelve un mensaje correspondiente.
* La función `factorial` calcula el factorial del número dado.
* La función `máximo_común_divisor` calcula el máximo común divisor de los dos números dados.
* La función `menor_común_múltiplo` calcula el menor común múltiplo de los dos números dados.
* En la sección de ejemplo de uso, se muestran los resultados de llamar a las funciones con algunos valores de entrada.