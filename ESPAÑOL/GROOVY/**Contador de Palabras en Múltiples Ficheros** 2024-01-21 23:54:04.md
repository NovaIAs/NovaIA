```groovy
// Importamos las clases necesarias
import groovy.io.FileType
import java.nio.file.Files
import java.nio.file.Paths

// Definimos el directorio donde se encuentran los ficheros a procesar
def directorio = "/ruta/al/directorio"

// Obtenemos una lista de todos los ficheros del directorio
def ficheros = Files.list(Paths.get(directorio)).filter { it.toFile().isFile() }.toList()

// Creamos un mapa para almacenar los resultados
def resultados = [:]

// Recorremos los ficheros
ficheros.each { fichero ->
    // Leemos el fichero
    def texto = Files.readString(fichero)

    // Dividimos el texto en líneas
    def lineas = texto.split("\n")

    // Recorremos las líneas
    lineas.each { linea ->
        // Eliminamos los espacios en blanco
        def lineaSinEspacios = linea.trim()

        // Si la línea no está vacía
        if (!lineaSinEspacios.isEmpty()) {
            // Dividimos la línea en palabras
            def palabras = lineaSinEspacios.split("\\s+")

            // Recorremos las palabras
            palabras.each { palabra ->
                // Contabilizamos la palabra
                resultados[palabra] = resultados.getOrDefault(palabra, 0) + 1
            }
        }
    }
}

// Ordenamos los resultados por número de apariciones
def resultadosOrdenados = resultados.sort { it.value }

// Imprimimos los resultados
println "Palabras más utilizadas:"
resultadosOrdenados.each { palabra, apariciones ->
    println "$palabra: $apariciones"
}
```

Explicación del código:

1. Importamos las clases necesarias.
2. Definimos el directorio donde se encuentran los ficheros a procesar.
3. Obtenemos una lista de todos los ficheros del directorio.
4. Creamos un mapa para almacenar los resultados.
5. Recorremos los ficheros.
6. Leemos el fichero.
7. Dividimos el texto en líneas.
8. Recorremos las líneas.
9. Eliminamos los espacios en blanco.
10. Si la línea no está vacía:
    * Dividimos la línea en palabras.
    * Recorremos las palabras.
    * Contabilizamos la palabra.
11. Ordenamos los resultados por número de apariciones.
12. Imprimimos los resultados.

Este código nos permite contar el número de apariciones de cada palabra en un conjunto de ficheros y mostrar las palabras más utilizadas.