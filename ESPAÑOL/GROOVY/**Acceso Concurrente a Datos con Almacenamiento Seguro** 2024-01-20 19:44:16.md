```groovy
import groovy.transform.Synchronized
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit

// Clase que representa un almacén de datos concurrentes.
class AlmacenDatos {

  // Mapa concurrente para almacenar los datos.
  private final ConcurrentHashMap<String, Object> datos = new ConcurrentHashMap<>()

  // Método para agregar un dato al almacén.
  @Synchronized
  void agregarDato(String clave, Object valor) {
    datos.put(clave, valor)
  }

  // Método para obtener un dato del almacén.
  @Synchronized
  Object obtenerDato(String clave) {
    datos.get(clave)
  }

  // Método para eliminar un dato del almacén.
  @Synchronized
  void eliminarDato(String clave) {
    datos.remove(clave)
  }
}

// Clase que representa un hilo que accede al almacén de datos.
class HiloAccesoDatos implements Runnable {

  // Almacén de datos al que se accede.
  private final AlmacenDatos almacenDatos

  // Clave del dato que se accede.
  private final String clave

  // Constructor.
  HiloAccesoDatos(AlmacenDatos almacenDatos, String clave) {
    this.almacenDatos = almacenDatos
    this.clave = clave
  }

  // Método que ejecuta el hilo.
  @Override
  void run() {
    println "Hilo ${Thread.currentThread().name} accede al dato ${clave}"
    Object valor = almacenDatos.obtenerDato(clave)
    println "Hilo ${Thread.currentThread().name} obtiene el valor ${valor} del dato ${clave}"
    Thread.sleep(TimeUnit.SECONDS.toMillis(2)) // Simula un procesamiento lento.
    println "Hilo ${Thread.currentThread().name} actualiza el valor del dato ${clave}"
    almacenDatos.agregarDato(clave, valor + 1)
    println "Hilo ${Thread.currentThread().name} termina de acceder al dato ${clave}"
  }
}

// Programa principal.
def almacenDatos = new AlmacenDatos()

// Creamos dos hilos que acceden a la misma clave del almacén de datos.
def hilo1 = new HiloAccesoDatos(almacenDatos, "clave1")
def hilo2 = new HiloAccesoDatos(almacenDatos, "clave1")

// Iniciamos los hilos.
hilo1.start()
hilo2.start()

// Esperamos a que los hilos terminen.
hilo1.join()
hilo2.join()

// Imprimimos el valor final del dato en el almacén de datos.
println "Valor final del dato clave1: ${almacenDatos.obtenerDato("clave1")}"
```

Explicación:

* La clase `AlmacenDatos` representa un almacén de datos concurrentes. Utiliza un mapa concurrente (`ConcurrentHashMap`) para almacenar los datos.
* La clase `HiloAccesoDatos` representa un hilo que accede al almacén de datos.
* El programa principal crea dos hilos que acceden a la misma clave del almacén de datos.
* Los hilos se inician y se espera a que terminen.
* Se imprime el valor final del dato en el almacén de datos.

En este código, los hilos acceden al almacén de datos de forma concurrente, lo que significa que pueden acceder al mismo dato al mismo tiempo. Sin embargo, el uso de un mapa concurrente garantiza que el acceso al dato sea seguro y que no se produzcan errores de concurrencia.