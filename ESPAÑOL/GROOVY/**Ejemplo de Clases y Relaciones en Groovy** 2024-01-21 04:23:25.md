```groovy
// Definición de clases y sus relaciones

class Persona {
    String nombre
    String apellido
}

class Direccion {
    String calle
    String ciudad
    String estado
    String codigoPostal
}

class Vehiculo {
    String marca
    String modelo
    int año
}

// Creación de instancias de las clases

def persona1 = new Persona(nombre: "Juan", apellido: "Pérez")
def direccion1 = new Direccion(calle: "Calle Principal", ciudad: "Ciudad del Este", estado: "Estado 1", codigoPostal: "12345")
def vehiculo1 = new Vehiculo(marca: "Toyota", modelo: "Camry", año: 2020)

// Asociación de instancias

persona1.direccion = direccion1
persona1.vehiculo = vehiculo1

// Impresión de la información

println "Nombre: ${persona1.nombre} ${persona1.apellido}"
println "Dirección: ${persona1.direccion.calle}, ${persona1.direccion.ciudad}, ${persona1.direccion.estado}, ${persona1.direccion.codigoPostal}"
println "Vehículo: ${persona1.vehiculo.marca} ${persona1.vehiculo.modelo} (${persona1.vehiculo.año})"

```

Explicación del código:

1. Definición de clases: Se definen tres clases: `Persona`, `Direccion` y `Vehiculo`, que representan los objetos que se van a crear y manipular en el código.

2. Creación de instancias: Se crean instancias de cada una de las clases, asignando valores a sus propiedades.

3. Asociación de instancias: Se asocian las instancias de las clases entre sí, utilizando la propiedad `direccion` y `vehiculo` de la clase `Persona`.

4. Impresión de la información: Se imprime la información de la persona, su dirección y su vehículo.