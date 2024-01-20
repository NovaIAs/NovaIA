```groovy
// Definición de una clase anidada

class Exterior {
  
  // Atributos
  private String nombre
  private List<Interior> interiores = []

  // Constructor
  Exterior(String nombre, List<Interior> interiores) {
    this.nombre = nombre
    this.interiores = interiores
  }

  // Métodos
  String getNombre() {
    return nombre
  }

  List<Interior> getInteriores() {
    return interiores
  }

  void añadirInterior(Interior interior) {
    interiores.add(interior)
  }

  void eliminarInterior(Interior interior) {
    interiores.remove(interior)
  }

  @Override
  String toString() {
    return "Exterior{nombre='$nombre', interiores=$interiores}"
  }
}

// Definición de una clase anidada

class Interior {
  
  // Atributos
  private String nombre
  private String descripción

  // Constructor
  Interior(String nombre, String descripción) {
    this.nombre = nombre
    this.descripción = descripción
  }

  // Métodos
  String getNombre() {
    return nombre
  }

  String getDescripción() {
    return descripción
  }

  @Override
  String toString() {
    return "Interior{nombre='$nombre', descripción='$descripción'}"
  }
}

// Creación de objetos

Exterior casa = new Exterior("Casa", [])
Interior sala = new Interior("Sala", "Es la habitación principal de la casa")
Interior cocina = new Interior("Cocina", "Es donde se prepara la comida")
Interior dormitorio = new Interior("Dormitorio", "Es donde se duerme")

// Añadir interiores a la casa

casa.añadirInterior(sala)
casa.añadirInterior(cocina)
casa.añadirInterior(dormitorio)

// Imprimir la información de la casa

println "Nombre de la casa: ${casa.nombre}"
println "Interiores de la casa:"
casa.interiores.each { interior ->
  println " - ${interior.nombre}: ${interior.descripción}"
}
```

Este código define dos clases anidadas, `Exterior` e `Interior`. La clase `Exterior` representa el exterior de una casa, mientras que la clase `Interior` representa el interior de una casa.

La clase `Exterior` tiene los siguientes atributos:

* `nombre`: El nombre de la casa.
* `interiores`: Una lista de objetos de la clase `Interior`.

La clase `Interior` tiene los siguientes atributos:

* `nombre`: El nombre del interior.
* `descripción`: Una descripción del interior.

El código también crea tres objetos de la clase `Interior`:

* `sala`: La sala de estar.
* `cocina`: La cocina.
* `dormitorio`: El dormitorio.

Luego, el código añade los objetos de la clase `Interior` al objeto de la clase `Exterior`.

Finalmente, el código imprime la información de la casa, incluyendo el nombre de la casa y una lista de los interiores de la casa.