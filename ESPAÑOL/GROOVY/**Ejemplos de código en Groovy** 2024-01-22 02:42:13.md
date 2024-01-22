```groovy
// Definimos una lista de nombres.
def nombres = ['Juan', 'María', 'Pedro', 'Ana', 'José']

// Usamos el método `each` para iterar sobre la lista de nombres.
nombres.each { nombre ->
  // Por cada nombre, imprimimos un mensaje.
  println "Hola, ${nombre}!"
}

// Definimos un mapa de nombres y edades.
def edades = [
  'Juan': 20,
  'María': 21,
  'Pedro': 22,
  'Ana': 23,
  'José': 24
]

// Usamos el método `each` para iterar sobre el mapa de nombres y edades.
edades.each { nombre, edad ->
  // Por cada par nombre-edad, imprimimos un mensaje.
  println "${nombre} tiene ${edad} años."
}

// Definimos una función para calcular el factorial de un número.
def factorial(numero) {
  // Si el número es menor o igual a 1, devolvemos 1.
  if (numero <= 1) {
    return 1
  }
  // Si el número es mayor que 1, calculamos el factorial recursivamente.
  else {
    return numero * factorial(numero - 1)
  }
}

// Imprimimos el factorial de 5.
println "El factorial de 5 es ${factorial(5)}."

// Definimos una clase `Persona`.
class Persona {
  // Definimos los atributos de la clase.
  String nombre
  int edad

  // Definimos el constructor de la clase.
  Persona(String nombre, int edad) {
    this.nombre = nombre
    this.edad = edad
  }

  // Definimos un método para obtener el nombre de la persona.
  String getNombre() {
    return nombre
  }

  // Definimos un método para obtener la edad de la persona.
  int getEdad() {
    return edad
  }
}

// Creamos una instancia de la clase `Persona`.
def persona = new Persona('Juan', 20)

// Imprimimos el nombre y la edad de la persona.
println "Nombre: ${persona.getNombre()}"
println "Edad: ${persona.getEdad()}"
```

Explicación del código:

* **Definición de la lista de nombres:** Definimos una lista de nombres usando la sintaxis `['Juan', 'María', 'Pedro', 'Ana', 'José']`.
* **Iteración sobre la lista de nombres:** Utilizamos el método `each` para iterar sobre la lista de nombres. El método `each` toma un bloque de código como argumento, y ejecuta ese bloque de código para cada elemento de la lista. En este caso, el bloque de código simplemente imprime un mensaje para cada nombre.
* **Definición del mapa de nombres y edades:** Definimos un mapa de nombres y edades usando la sintaxis `['Juan': 20, 'María': 21, 'Pedro': 22, 'Ana': 23, 'José': 24]`. Los mapas en Groovy son similares a los diccionarios en Python o los objetos en JavaScript.
* **Iteración sobre el mapa de nombres y edades:** Utilizamos el método `each` para iterar sobre el mapa de nombres y edades. El método `each` toma un bloque de código como argumento, y ejecuta ese bloque de código para cada par clave-valor del mapa. En este caso, el bloque de código simplemente imprime un mensaje para cada par nombre-edad.
* **Definición de la función factorial:** Definimos una función llamada `factorial` que calcula el factorial de un número. La función `factorial` toma un número como argumento y devuelve el factorial de ese número. El factorial de un número es el producto de todos los números enteros positivos menores o iguales a ese número.
* **Uso de la función factorial:** Llamamos a la función `factorial` con el argumento 5 y imprimimos el resultado.
* **Definición de la clase Persona:** Definimos una clase llamada `Persona` que representa a una persona. La clase `Persona` tiene dos atributos: `nombre` y `edad`.
* **Definición del constructor de la clase Persona:** Definimos el constructor de la clase `Persona` que toma dos argumentos: `nombre` y `edad`. El constructor inicializa los atributos `nombre` y `edad` con los valores de los argumentos pasados.
* **Definición de los métodos getNombre y getEdad:** Definimos dos métodos en la clase `Persona`: `getNombre` y `getEdad`. El método `getNombre` devuelve el valor del atributo `nombre` y el método `getEdad` devuelve el valor del atributo `edad`.
* **Creación de una instancia de la clase Persona:** Creamos una instancia de la clase `Persona` usando la sintaxis `new Persona('Juan', 20)`.
* **Uso de los métodos getNombre y getEdad:** Llamamos a los métodos `getNombre` y `getEdad` para obtener el nombre y la edad de la persona, y los imprimimos.