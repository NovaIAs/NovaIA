```groovy
// Definición de una clase "Cancion" con propiedades y métodos.
class Cancion {
    String titulo
    String artista
    int año
    int duración

    // Constructor para inicializar las propiedades de la canción.
    Cancion(String titulo, String artista, int año, int duración) {
        this.titulo = titulo
        this.artista = artista
        this.año = año
        this.duración = duración
    }

    // Definición de métodos para obtener y establecer las propiedades de la canción.
    String getTitulo() {
        return titulo
    }

    void setTitulo(String titulo) {
        this.titulo = titulo
    }

    String getArtista() {
        return artista
    }


    void setArtista(String artista) {
        this.artista = artista
    }

    int getAño() {
        return año
    }

    void setAño(int año) {
        this.año = año
    }

    int getDuración() {
        return duración
    }

    void setDuración(int duración) {
        this.duración = duración
    }
    

    // Definición de un método toString() para mostrar la información de la canción.
    String toString() {
        return "Canción: ${titulo}, Artista: ${artista}, Año: ${año}, Duración: ${duración}"
    }
}

// Creación de una lista de canciones.
List<Cancion> canciones = []

// Añadiendo canciones a la lista.
canciones.add(new Cancion("Bohemian Rhapsody", "Queen", 1975, 540))
canciones.add(new Cancion("Imagine", "John Lennon", 1971, 308))
canciones.add(new Cancion("Stairway to Heaven", "Led Zeppelin", 1971, 802))
canciones.add(new Cancion("Hotel California", "Eagles", 1977, 636))
canciones.add(new Cancion("Thunderstruck", "AC/DC", 1990, 432))

// Imprimir las canciones de la lista.
canciones.each { println it }

// Utilización de expresiones lambda y métodos de ordenación para ordenar las canciones por duración.
List<Cancion> cancionesOrdenadasPorDuracion = canciones.sort { a, b -> a.duración <=> b.duración }

// Imprimir las canciones ordenadas.
cancionesOrdenadasPorDuracion.each { println it }

// Utilización de colecciones y métodos de filtrado para encontrar las canciones del año 1971.
List<Cancion> cancionesDe1971 = canciones.findAll { it.año == 1971 }

// Imprimir las canciones del año 1971.
cancionesDe1971.each { println it }

// Utilización de expresiones lambda y métodos de filtrado para encontrar las canciones con duración mayor a 500 segundos.
List<Cancion> cancionesMasDe500Segundos = canciones.findAll { it.duración > 500 }

// Imprimir las canciones con duración mayor a 500 segundos.
cancionesMasDe500Segundos.each { println it }

// Utilización de expresiones lambda y métodos de transformación para obtener los títulos de las canciones.
List<String> titulosDeCanciones = canciones.collect { it.titulo }

// Imprimir los títulos de las canciones.
titulosDeCanciones.each { println it }
```

Explicación del código:

- **Definición de la clase Canción:** Se define una clase llamada "Cancion" con cuatro propiedades: título, artista, año y duración. También se definen métodos para obtener y establecer estas propiedades, así como un método toString() para mostrar la información de la canción.


- **Creación de una lista de canciones:** Se crea una lista llamada "canciones" y se añaden cinco canciones a la lista, cada una con sus propias propiedades de título, artista, año y duración.


- **Impresión de las canciones de la lista:** Se imprime cada canción de la lista utilizando el método toString() definido en la clase Canción.


- **Ordenación de las canciones por duración:** Se utiliza el método de ordenación sort() junto con una expresión lambda para ordenar las canciones por su duración.


- **Obtención de las canciones de 1971:** Se utiliza el método de filtrado findAll() junto con una expresión lambda para encontrar las canciones que fueron publicadas en el año 1971.


- **Obtención de las canciones con duración mayor a 500 segundos:** Se utiliza el método de filtrado findAll() junto con una expresión lambda para encontrar las canciones cuya duración es mayor de 500 segundos.


- **Obtención de los títulos de las canciones:** Se utiliza el método de transformación collect() junto con una expresión lambda para obtener una lista de los títulos de las canciones.


- **Impresión de los títulos de las canciones:** Se imprime cada título de canción de la lista de títulos de canciones.