```groovy
// Paquete donde se encuentra nuestro código
package com.ejemplo.groovy

// Clase principal de nuestro programa
class ProgramaPrincipal {

    // Función principal que se ejecuta al iniciar el programa
    static void main(String[] args) {
        // Creamos una lista con los nombres de los meses del año
        def meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"]

        // Creamos una lista con los números del 1 al 12
        def numeros = (1..12).toList()

        // Creamos un mapa que asocia cada número de mes con su nombre correspondiente
        def mapaMeses = [:].withDefault { k -> meses[k - 1] } // Se usa de esta manera para prevenir errores de índice

        // Imprimimos una tabla con los números y los nombres de los meses
        println "Tabla de Números y Meses:"
        numeros.each { numero ->
            println "$numero\t${mapaMeses[numero]}"
        }

        // Creamos una lista con los cuadrados de los números del 1 al 10
        def cuadrados = (1..10).collect { it * it }

        // Imprimimos los cuadrados
        println "Cuadrados de los Números del 1 al 10:"
        cuadrados.each { cuadrado ->
            println cuadrado
        }

        // Creamos una lista con los nombres de las frutas que empiezan por la letra "M"
        def frutasConM = ["Manzana", "Melocotón", "Mandarina", "Mango", "Mora"].findAll { it[0] == 'M' }

        // Imprimimos las frutas que empiezan por la letra "M"
        println "Frutas que empiezan por la letra \"M\":"
        frutasConM.each { fruta ->
            println fruta
        }

        // Buscamos el índice del número 7 en la lista de números
        def indiceDelSiete = numeros.indexOf(7)

        // Imprimimos el índice del número 7
        println "Índice del número 7 en la lista de números: $indiceDelSiete"

        // Creamos una lista con los nombres de los meses que tienen 31 días
        def mesesCon31Dias = meses.findAll { it.length() == 7 }

        // Imprimimos los nombres de los meses que tienen 31 días
        println "Meses que tienen 31 días:"
        mesesCon31Dias.each { mes ->
            println mes
        }

        // Creamos una lista con los números pares del 1 al 20
        def numerosPares = (1..20).findAll { it % 2 == 0 }

        // Imprimimos los números pares del 1 al 20
        println "Números pares del 1 al 20:"
        numerosPares.each { numero ->
            println numero
        }
    }
}
```

Este código es un programa complejo en Groovy que realiza una variedad de tareas, incluyendo:

* Crea una lista con los nombres de los meses del año.
* Crea una lista con los números del 1 al 12.
* Crea un mapa que asocia cada número de mes con su nombre correspondiente.
* Imprime una tabla con los números y los nombres de los meses.
* Crea una lista con los cuadrados de los números del 1 al 10.
* Imprime los cuadrados.
* Crea una lista con los nombres de las frutas que empiezan por la letra "M".
* Imprime las frutas que empiezan por la letra "M".
* Busca el índice del número 7 en la lista de números.
* Imprime el índice del número 7.
* Crea una lista con los nombres de los meses que tienen 31 días.
* Imprime los nombres de los meses que tienen 31 días.
* Crea una lista con los números pares del 1 al 20.
* Imprime los números pares del 1 al 20.

Es un código complejo y difícil de entender, pero es un buen ejemplo de las capacidades de Groovy.