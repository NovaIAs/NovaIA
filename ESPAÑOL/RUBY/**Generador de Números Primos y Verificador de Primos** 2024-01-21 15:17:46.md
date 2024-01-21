```ruby
# Importa la biblioteca "highline" para manejar la entrada del usuario en la consola.
require 'highline/import'

# Solicita al usuario que introduzca un número.
puts "Introduce un número:"
numero = gets.chomp.to_i

# Genera una lista de los primeros 10 números primos.
primos = []
contador = 2
while primos.length < 10:
  es_primo = true
  for i in range(2, contador/2+1):
    if contador % i == 0:
      es_primo = false
  if es_primo:
    primos.append(contador)
  contador += 1

# Imprime el número introducido por el usuario.
puts "El número que introdujiste es:", numero

# Comprueba si el número introducido por el usuario es primo.
es_primo = true
for i in range(2, numero/2+1):
  if numero % i == 0:
    es_primo = false
    break

# Imprime un mensaje indicando si el número introducido por el usuario es primo o no.
if es_primo:
  puts "El número", numero, "es primo."
else:
  puts "El número", numero, "no es primo."

# Imprime la lista de los primeros 10 números primos.
puts "Los primeros 10 números primos son:"
for primo in primos:
  print primo, " "
puts
```

Explicación del código:

- Se importa la biblioteca "highline" para manejar la entrada del usuario en la consola.
- Se solicita al usuario que introduzca un número.
- Se genera una lista de los primeros 10 números primos.
- Se imprime el número introducido por el usuario.
- Se comprueba si el número introducido por el usuario es primo.
- Se imprime un mensaje indicando si el número introducido por el usuario es primo o no.
- Se imprime la lista de los primeros 10 números primos.