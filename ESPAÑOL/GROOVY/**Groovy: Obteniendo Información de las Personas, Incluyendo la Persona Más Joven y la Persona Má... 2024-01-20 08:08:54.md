```groovy
// Definir una lista de nombres
def nombres = ['Juan', 'María', 'Pedro', 'Ana', 'José']

// Definir un mapa de edades
def edades = [Juan: 20, María: 25, Pedro: 30, Ana: 35, José: 40]

// Definir una función para obtener el nombre y la edad de una persona por su índice
def getPersona(index) {
  def nombre = nombres[index]
  def edad = edades[nombre]
  return "${nombre} (${edad} años)"
}

// Definir una función para obtener la persona más joven y la persona más vieja
def getPersonasExtremas() {
  def minEdad = edades.values().min()
  def maxEdad = edades.values().max()

  def personaMasJoven = getPersona(edades.findIndexOfValue(minEdad))
  def personaMasVieja = getPersona(edades.findIndexOfValue(maxEdad))

  return [personaMasJoven, personaMasVieja]
}

// Imprimir el nombre y la edad de cada persona
nombres.each { nombre ->
  println "${nombre} (${edades[nombre]} años)"
}

// Imprimir la persona más joven y la persona más vieja
println "Persona más joven: ${getPersonasExtremas()[0]}"
println "Persona más vieja: ${getPersonasExtremas()[1]}"
```

Explicación del código:

* Se define una lista de nombres y un mapa de edades.
* Se define una función `getPersona` que devuelve el nombre y la edad de una persona por su índice.
* Se define una función `getPersonasExtremas` que devuelve la persona más joven y la persona más vieja.
* Se imprime el nombre y la edad de cada persona.
* Se imprime la persona más joven y la persona más vieja.

El código utiliza varias características de Groovy, como:

* Cierres (closures)
* Métodos funcionales (findIndexOfValue)
* Interpolación de cadenas (strings)

Esto hace que el código sea más conciso y expresivo.