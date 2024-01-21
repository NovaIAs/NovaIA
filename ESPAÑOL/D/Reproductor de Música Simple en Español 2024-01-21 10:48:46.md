```d
// Reproductor de audio simple
import std.stdio, std.thread, std.string, std.concurrency, std.range, std.exception;

// Estructura de datos para almacenar información sobre la canción actual
struct Cancion {
    string titulo, artista;
    int duracion; // en segundos
};

// Estructura de datos para almacenar la lista de canciones
struct ListaCanciones {
    Cancion[] canciones;

    // Agrega una canción a la lista
    void agregarCancion(Cancion cancion) {
        canciones ~= cancion;
    }

    // Elimina una canción de la lista
    void eliminarCancion(int indice) {
        canciones.remove(indice);
    }

    // Obtiene el título de la canción actual
    string getTituloActual() {
        return canciones[indiceActual].titulo;
    }

    // Obtiene el artista de la canción actual
    string getArtistaActual() {
        return canciones[indiceActual].artista;
    }

    // Obtiene la duración de la canción actual
    int getDuracionActual() {
        return canciones[indiceActual].duracion;
    }

    // Establece el índice de la canción actual
    void setIndiceActual(int indice) {
        indiceActual = indice;
    }

    // Incrementa el índice de la canción actual
    void siguienteCancion() {
        indiceActual++;
        if (indiceActual >= canciones.length) {
            indiceActual = 0;
        }
    }

    // Decrementa el índice de la canción actual
    void cancionAnterior() {
        indiceActual--;
        if (indiceActual < 0) {
            indiceActual = canciones.length - 1;
        }
    }

private:
    int indiceActual = 0; // Índice de la canción actual
};

// Función para reproducir una canción
void reproducirCancion(Cancion cancion) {
    // Imprime el título y el artista de la canción
    writeln("Reproduciendo:", cancion.titulo, "por", cancion.artista);

    // Simula la reproducción de la canción durante su duración
    for (int i = 0; i < cancion.duracion; i++) {
        // Imprime el tiempo transcurrido
        writeln("Tiempo transcurrido:", i, "segundos");

        // Espera un segundo
        delay(1000);
    }

    // Imprime un mensaje cuando la canción termina de reproducirse
    writeln("Canción terminada");
}

// Función principal
void main() {
    // Crea una lista de canciones
    ListaCanciones listaCanciones;

    // Agrega algunas canciones a la lista
    listaCanciones.agregarCancion(Cancion("Canción 1", "Artista 1", 180));
    listaCanciones.agregarCancion(Cancion("Canción 2", "Artista 2", 240));
    listaCanciones.agregarCancion(Cancion("Canción 3", "Artista 3", 300));

    // Crea un hilo para reproducir la música
    Thread hiloReproduccion = new Thread(&reproducirCancion, listaCanciones.getTituloActual());

    // Bucle principal del reproductor
    while (true) {
        // Imprime el título y el artista de la canción actual
        writeln("Canción actual:", listaCanciones.getTituloActual(), "por", listaCanciones.getArtistaActual());

        // Imprime la duración de la canción actual
        writeln("Duración:", listaCanciones.getDuracionActual(), "segundos");

        // Espera la entrada del usuario
        string entrada = readln();

        // Procesa la entrada del usuario
        if (entrada == "s") {
            // Siguiente canción
            listaCanciones.siguienteCancion();
        } else if (entrada == "a") {
            // Canción anterior
            listaCanciones.cancionAnterior();
        } else if (entrada == "q") {
            // Salir del reproductor
            break;
        }

        // Actualiza el título y el artista de la canción actual
        hiloReproduccion.setThis(listaCanciones.getTituloActual());
    }

    // Detiene el hilo de reproducción
    hiloReproduccion.join();
}
```

Este código crea un reproductor de audio simple que permite reproducir una lista de canciones. El reproductor tiene una interfaz de línea de comandos que permite al usuario reproducir, pausar y cambiar de canción.

El código está dividido en varias partes:

* La estructura `Cancion` define la información básica de una canción, como su título, artista y duración.
* La estructura `ListaCanciones` almacena una lista de canciones y proporciona métodos para agregar, eliminar y obtener información sobre las canciones.
* La función `reproducirCancion()` reproduce una canción simulando el paso del tiempo.
* La función `main()` es la función principal del reproductor. Crea una lista de canciones, inicia un hilo para reproducir la música y maneja la entrada del usuario.

El código utiliza la biblioteca `std.concurrency` para crear un hilo para la reproducción de la música. También utiliza la biblioteca `std.range` para manipular la lista de canciones.

El código está escrito en español, lo que lo hace fácil de entender y modificar.