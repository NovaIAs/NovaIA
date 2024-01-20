```groovy
// Definición de clases y propiedades

class Persona {
    String nombre
    String apellido
    Integer edad
}

class Auto {
    String marca
    String modelo
    Integer año
}

class Direccion {
    String calle
    String ciudad
    String estado
    String códigoPostal
}

// Creación de objetos

Persona persona = new Persona(nombre: "Juan", apellido: "Pérez", edad: 25)
Auto auto = new Auto(marca: "Toyota", modelo: "Corolla", año: 2018)
Direccion direccion = new Direccion(calle: "Calle 123", ciudad: "Ciudad de México", estado: "Distrito Federal", códigoPostal: "01234")

// Uso de objetos en una lista

List<Persona> personas = [persona]
List<Auto> autos = [auto]
List<Direccion> direcciones = [direccion]

// Recorrido de listas con expresiones lambda

personas.each { println "Nombre: ${it.nombre}, Apellido: ${it.apellido}" }
autos.each { println "Marca: ${it.marca}, Modelo: ${it.modelo}, Año: ${it.año}" }
direcciones.each { println "Calle: ${it.calle}, Ciudad: ${it.ciudad}, Estado: ${it.estado}, Código Postal: ${it.códigoPostal}" }

// Uso de expresiones lambda en un contexto más complejo

personas.findAll { it.edad > 21 }.each { println "Nombre: ${it.nombre}, Apellido: ${it.apellido}, Edad: ${it.edad}" }
autos.filter { it.año > 2010 }.each { println "Marca: ${it.marca}, Modelo: ${it.modelo}, Año: ${it.año}" }
direcciones.sort { it.estado <=> it.ciudad <=> it.calle }.each { println "Calle: ${it.calle}, Ciudad: ${it.ciudad}, Estado: ${it.estado}, Código Postal: ${it.códigoPostal}" }

// Uso de expresiones lambda en una función

def sumar(a, b) { a + b }
def resultado = sumar(10, 20)
println "Resultado: $resultado"

// Uso de expresiones lambda en una clase

class Calculadora {
    def sumar(a, b) { a + b }
    def restar(a, b) { a - b }
    def multiplicar(a, b) { a * b }
    def dividir(a, b) { a / b }
}

Calculadora calculadora = new Calculadora()
println "Suma: ${calculadora.sumar(10, 20)}"
println "Resta: ${calculadora.restar(10, 20)}"
println "Multiplicación: ${calculadora.multiplicar(10, 20)}"
println "División: ${calculadora.dividir(10, 20)}"
```

Explicación del código:

* Se definen tres clases: `Persona`, `Auto` y `Direccion`, cada una con sus propias propiedades.
* Se crean objetos de cada una de las clases y se almacenan en listas.
* Se utilizan expresiones lambda para recorrer las listas y realizar operaciones en los objetos.
* Se utiliza una expresión lambda en un contexto más complejo para filtrar y ordenar objetos.
* Se utiliza una expresión lambda en una función para realizar una operación aritmética.
* Se utiliza una expresión lambda en una clase para definir métodos que realizan operaciones aritméticas.