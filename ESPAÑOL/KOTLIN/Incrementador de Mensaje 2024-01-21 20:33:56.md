```kotlin
// Importamos la librería de Android para crear una ventana y trabajar con vistas
import android.app.Activity
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.TextView

// Definimos una nueva clase llamada MainActivity que extiende de la clase Activity
class MainActivity : Activity() {

    // Declaramos las variables que usaremos en nuestra aplicación
    private lateinit var tvMensaje: TextView
    private lateinit var btnIncrementar: Button

    // Este método se ejecuta cuando se crea la actividad
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Establecemos el layout de la actividad
        setContentView(R.layout.activity_main)

        // Obtenemos las referencias a las vistas que usaremos
        tvMensaje = findViewById(R.id.tvMensaje)
        btnIncrementar = findViewById(R.id.btnIncrementar)

        // Añadimos un listener al botón para que cuando se haga click en él, se incremente el valor del mensaje
        btnIncrementar.setOnClickListener {
            var mensajeActual = tvMensaje.text.toString()

            // Si el mensaje actual es vacío, lo inicializamos a 0
            if (mensajeActual.isEmpty()) {
                mensajeActual = "0"
            }

            // Incrementamos el valor del mensaje
            val nuevoMensaje = (mensajeActual.toInt() + 1).toString()

            // Actualizamos el texto del mensaje en la vista
            tvMensaje.text = nuevoMensaje
        }
    }
}
```

Explicación del código:

* Importamos la librería de Android para crear una ventana y trabajar con vistas.
* Definimos una nueva clase llamada MainActivity que extiende de la clase Activity. Esto significa que nuestra clase es una actividad, que es un componente básico de una aplicación de Android.
* Declaramos las variables que usaremos en nuestra aplicación. En este caso, tenemos dos variables: tvMensaje y btnIncrementar. Estas variables se inicializarán en el método onCreate() cuando se cree la actividad.
* El método onCreate() se ejecuta cuando se crea la actividad. En este método, establecemos el layout de la actividad, obtenemos las referencias a las vistas que usaremos y añadimos un listener al botón para que cuando se haga click en él, se incremente el valor del mensaje.
* El método setContentView() establece el layout de la actividad. En este caso, estamos usando el layout activity_main.xml.
* El método findViewById() obtiene la referencia a una vista por su ID. En este caso, estamos obteniendo las referencias a las vistas tvMensaje y btnIncrementar.
* El método setOnClickListener() añade un listener a una vista para que cuando se haga click en ella, se ejecute el código especificado en el listener. En este caso, cuando se haga click en el botón btnIncrementar, se ejecutará el código que está dentro del listener.
* En el listener del botón btnIncrementar, primero obtenemos el valor actual del mensaje y lo convertimos a un entero. Si el valor actual del mensaje es vacío, lo inicializamos a 0.
* A continuación, incrementamos el valor del mensaje en 1 y lo convertimos de nuevo a una cadena de texto.
* Por último, actualizamos el texto del mensaje en la vista tvMensaje.

Este código crea una aplicación de Android sencilla que muestra un mensaje en una vista de texto y tiene un botón para incrementar el valor del mensaje.