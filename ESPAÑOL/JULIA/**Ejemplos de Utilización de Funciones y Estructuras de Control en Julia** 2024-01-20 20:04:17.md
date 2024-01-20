```julia
# Definir una función para calcular el factorial de un número
factorial(n) = n == 0 ? 1 : n * factorial(n-1)

# Crear una lista de números del 1 al 10
numeros = 1:10

# Aplicar la función factorial a cada número de la lista
factoriales = map(factorial, numeros)

# Imprimir la lista de factoriales
println("Factoriales:")
println(factoriales)

# Definir una función para calcular la suma de los dígitos de un número
suma_digitos(n) = reduce(+, map(Int, string(n)))

# Crear una lista de números del 1 al 100
numeros_hasta_100 = 1:100

# Aplicar la función suma_digitos a cada número de la lista
sumas_digitos = map(suma_digitos, numeros_hasta_100)

# Imprimir la lista de sumas de dígitos
println("Sumas de dígitos:")
println(sumas_digitos)

# Definir una función para contar el número de vocales en una cadena de texto
contar_vocales(cadena) = length(filter(x -> x in 'aeiou', cadena))

# Crear una lista de cadenas de texto
cadenas = ["Hola", "Mundo", "Cómo", "Estás"]

# Aplicar la función contar_vocales a cada cadena de texto de la lista
numero_vocales = map(contar_vocales, cadenas)

# Imprimir la lista de número de vocales
println("Número de vocales:")
println(numero_vocales)

```

Este código realiza varias operaciones matemáticas y de procesamiento de texto en Julia.

**1.** La primera parte del código define una función `factorial` que calcula el factorial de un número dado. Luego, crea una lista de números del 1 al 10 y aplica la función `factorial` a cada número de la lista. Los resultados se imprimen en la salida estándar.

**2.** La segunda parte del código define una función `suma_digitos` que calcula la suma de los dígitos de un número dado. Luego, crea una lista de números del 1 al 100 y aplica la función `suma_digitos` a cada número de la lista. Los resultados se imprimen en la salida estándar.

**3.** La tercera parte del código define una función `contar_vocales` que cuenta el número de vocales en una cadena de texto dada. Luego, crea una lista de cadenas de texto y aplica la función `contar_vocales` a cada cadena de texto de la lista. Los resultados se imprimen en la salida estándar.

Este código es un ejemplo de cómo se puede utilizar Julia para realizar una variedad de operaciones matemáticas y de procesamiento de texto de manera eficiente y concisa.