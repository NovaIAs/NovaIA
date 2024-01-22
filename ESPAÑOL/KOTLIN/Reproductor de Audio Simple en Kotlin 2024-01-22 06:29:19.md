```kotlin
// Definición de la interfaz Reproductor
interface Reproductor {
    fun reproducir(rutaArchivo: String)
    fun pausar()
    fun detener()
    fun obtenerDuracion(): Int
    fun obtenerPosicionActual(): Int
    fun buscar(posicion: Int)
    fun establecerVolumen(volumen: Int)
    fun obtenerVolumen(): Int
}

// Implementación de la interfaz Reproductor utilizando la API de Android
class ReproductorAndroid : Reproductor {

    private val mediaPlayer = MediaPlayer()

    override fun reproducir(rutaArchivo: String) {
        mediaPlayer.setDataSource(rutaArchivo)
        mediaPlayer.prepare()
        mediaPlayer.start()
    }

    override fun pausar() {
        mediaPlayer.pause()
    }

    override fun detener() {
        mediaPlayer.stop()
    }

    override fun obtenerDuracion(): Int {
        return mediaPlayer.duration
    }

    override fun obtenerPosicionActual(): Int {
        return mediaPlayer.currentPosition
    }

    override fun buscar(posicion: Int) {
        mediaPlayer.seekTo(posicion)
    }

    override fun establecerVolumen(volumen: Int) {
        mediaPlayer.setVolume(volumen / 100f, volumen / 100f)
    }

    override fun obtenerVolumen(): Int {
        return mediaPlayer.getVolume(MediaPlayer.VolumeType.MUSIC) * 100
    }
}

// Clase principal de la aplicación
class AplicacionReproductor {

    private val reproductor = ReproductorAndroid()

    fun reproducirArchivo(rutaArchivo: String) {
        reproductor.reproducir(rutaArchivo)
    }

    fun pausarReproduccion() {
        reproductor.pausar()
    }

    fun detenerReproduccion() {
        reproductor.detener()
    }

    fun obtenerDuracionArchivo(): Int {
        return reproductor.obtenerDuracion()
    }

    fun obtenerPosicionActual(): Int {
        return reproductor.obtenerPosicionActual()
    }

    fun buscarEnArchivo(posicion: Int) {
        reproductor.buscar(posicion)
    }

    fun establecerVolumen(volumen: Int) {
        reproductor.establecerVolumen(volumen)
    }

    fun obtenerVolumen(): Int {
        return reproductor.obtenerVolumen()
    }
}

// Ejemplo de uso de la aplicación
val aplicacion = AplicacionReproductor()
aplicacion.reproducirArchivo("/ruta/a/archivo.mp3")
aplicacion.pausarReproduccion()
aplicacion.obtenerDuracionArchivo()
aplicacion.obtenerPosicionActual()
aplicacion.buscarEnArchivo(10000)
aplicacion.establecerVolumen(50)
aplicacion.obtenerVolumen()
```

Explicación del código:

1. **Interfaz Reproductor**: Se define una interfaz `Reproductor` que contiene los métodos necesarios para controlar la reproducción de audio. Estos métodos incluyen reproducir, pausar, detener, obtener la duración, obtener la posición actual, buscar, establecer el volumen y obtener el volumen.
2. **Implementación de la interfaz Reproductor**: La clase `ReproductorAndroid` implementa la interfaz `Reproductor` utilizando la API de Android. Esta clase utiliza la clase `MediaPlayer` de Android para reproducir audio.
3. **Clase principal de la aplicación**: La clase `AplicacionReproductor` es la clase principal de la aplicación. Esta clase contiene un objeto de la clase `ReproductorAndroid` y expone métodos para controlar la reproducción de audio.
4. **Ejemplo de uso de la aplicación**: Se crea un objeto de la clase `AplicacionReproductor` y se utilizan sus métodos para reproducir un archivo de audio, pausar la reproducción, obtener la duración del archivo, obtener la posición actual, buscar en el archivo, establecer el volumen y obtener el volumen.