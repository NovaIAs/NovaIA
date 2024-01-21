```groovy
// Importación de bibliotecas necesarias
import java.util.Arrays

// Definición de una clase con una propiedad y un método
class Persona {
    String nombre
    String saludar() {
        return "Hola, mi nombre es $nombre."
    }
}

// Definición de una clase con un método estático
class Calculadora {
    static int sumar(int a, int b) {
        return a + b
    }
}

// Definición de una función anónima
def multiplicar = { int a, int b -> a * b }

// Creación de una lista de números
def numeros = [1, 2, 3, 4, 5]

// Uso de la propiedad y el método de la clase Persona
def persona = new Persona(nombre: "Juan")
println persona.saludar()

// Uso del método estático de la clase Calculadora
println Calculadora.sumar(10, 20)

// Uso de la función anónima para multiplicar
println multiplicar(10, 20)

// Uso de la lista de números para aplicar una transformación y obtener una nueva lista
def cuadrados = numeros.collect { n -> n * n }
println cuadrados

// Uso de la lista de números para aplicar una filtración y obtener una nueva lista
def pares = numeros.findAll { n -> n % 2 == 0 }
println pares

// Uso de la lista de números para aplicar una reducción y obtener un valor único
def sumaTotal = numeros.sum()
println sumaTotal

// Uso de la lista de números para aplicar una agrupación y obtener un mapa de grupos
def gruposPorParidad = numeros.groupBy { n -> n % 2 }
println gruposPorParidad

// Uso de la lista de números para aplicar una ordenación y obtener una nueva lista
def numerosOrdenados = numeros.sort()
println numerosOrdenados

// Uso de la lista de números para aplicar una paginación y obtener una nueva lista
def pagina1 = numeros.subList(0, 3)
println pagina1

// Uso de la lista de números para aplicar una partición y obtener dos nuevas listas
def (pares, impares) = numeros.partition { n -> n % 2 == 0 }
println pares
println impares

// Uso de la lista de números para aplicar una búsqueda y obtener un elemento
def numero3 = numeros.find { n -> n == 3 }
println numero3

// Uso de la lista de números para aplicar una búsqueda y obtener el índice de un elemento
def indiceDe5 = numeros.indexOf(5)
println indiceDe5

// Uso de la lista de números para aplicar una búsqueda y obtener el último elemento que cumpla una condición
def ultimoPar = numeros.findLast { n -> n % 2 == 0 }
println ultimoPar

// Uso de la lista de números para aplicar una búsqueda y obtener el índice del último elemento que cumpla una condición
def indiceDelUltimoPar = numeros.lastIndexOf { n -> n % 2 == 0 }
println indiceDelUltimoPar

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento
numeros.each { n -> println n }

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento y acumular el resultado
def sumaAcumulada = numeros.inject(0) { suma, n -> suma + n }
println sumaAcumulada

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero detener la iteración cuando se cumpla una condición
def primerPar = numeros.find { n -> n % 2 == 0 }
println primerPar

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero detener la iteración cuando se cumpla una condición y obtener el índice del elemento
def indiceDelPrimerPar = numeros.findIndex { n -> n % 2 == 0 }
println indiceDelPrimerPar

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero excluir los elementos que cumplan una condición
def numerosSin3 = numeros.findAll { n -> n != 3 }
println numerosSin3

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero excluir los elementos que cumplan una condición y obtener el índice del elemento
def indiceDelPrimer3 = numeros.findIndexOf { n -> n == 3 }
println indiceDelPrimer3

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero incluir los elementos que cumplan una condición
def numerosCon3 = numeros.findAll { n -> n == 3 }
println numerosCon3

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero incluir los elementos que cumplan una condición y obtener el índice del elemento
def indiceDelUltimo3 = numeros.findLastIndexOf { n -> n == 3 }
println indiceDelUltimo3

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero limitar el número de iteraciones
def primeros3Numeros = numeros.take(3)
println primeros3Numeros

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero omitir las primeras iteraciones
def ultimos3Numeros = numeros.drop(3)
println ultimos3Numeros

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero agrupar los elementos en sublistas de un tamaño determinado
def sublistasDe2Elementos = numeros.window(2)
println sublistasDe2Elementos

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero dividir la lista en sublistas de un tamaño determinado
def sublistasDe2Elementos = numeros.eachSlice(2)
println sublistasDe2Elementos

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento, pero omitir los elementos que cumplan una condición y agrupar los elementos restantes en sublistas de un tamaño determinado
def sublistasDe2ElementosSin3 = numeros.findAll { n -> n != 3 }.eachSlice(2)
println sublistasDe2ElementosSin3

// Uso de la lista de números para aplicar una iteración y realizar una operación en cada elemento,