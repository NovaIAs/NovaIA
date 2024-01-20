```groovy
// Definimos una función que toma una lista de números y devuelve la suma de los mismos
def sumar(numeros) {
  // Usamos el operador de propagación "..." para descomponer la lista en argumentos individuales
  def suma = 0
  for (numero in numeros) {
    suma += numero
  }
  return suma
}

// Definimos una clase llamada "Persona" con dos propiedades: "nombre" y "edad"
class Persona {
  String nombre
  int edad

  // Constructor para la clase "Persona" que toma el nombre y la edad como parámetros
  Persona(String nombre, int edad) {
    this.nombre = nombre
    this.edad = edad
  }

  // Definimos un método llamado "saludar" que devuelve un mensaje de saludo
  String saludar() {
    "Hola, mi nombre es ${nombre} y tengo ${edad} años."
  }
}

// Creamos una lista de números
def numeros = [1, 2, 3, 4, 5]

// Calculamos la suma de los números usando la función "sumar"
def sumaNumeros = sumar(numeros)

// Creamos una lista de personas
def personas = [
  new Persona("Juan", 25),
  new Persona("María", 30),
  new Persona("Pedro", 35)
]

// Iteramos sobre la lista de personas y saludamos a cada una de ellas
for (persona in personas) {
  println persona.saludar()
}

// Imprimimos la suma de los números y el saludo de cada persona
println "La suma de los números es: ${sumaNumeros}"
for (persona in personas) {
  println persona.saludar()
}
```

Explicación del código:

1. **Función `sumar`**: Esta función toma una lista de números y devuelve la suma de los mismos. Utiliza el operador de propagación "..." para descomponer la lista en argumentos individuales.
2. **Clase `Persona`**: Esta clase representa a una persona con dos propiedades: "nombre" y "edad". Tiene un constructor que toma el nombre y la edad como parámetros, y un método llamado "saludar" que devuelve un mensaje de saludo.
3. **Lista de números**: Creamos una lista de números del 1 al 5.
4. **Cálculo de la suma de los números**: Usamos la función "sumar" para calcular la suma de los números en la lista.
5. **Lista de personas**: Creamos una lista de personas, cada una de las cuales tiene un nombre y una edad.
6. **Iteración sobre la lista de personas**: Iteramos sobre la lista de personas y saludamos a cada una de ellas utilizando el método "saludar".
7. **Impresión de la suma de los números y el saludo de cada persona**: Imprimimos la suma de los números y el saludo de cada persona en la consola.