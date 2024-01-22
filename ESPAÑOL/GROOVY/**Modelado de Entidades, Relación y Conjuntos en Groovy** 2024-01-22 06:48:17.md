```groovy
// Definir una clase abstracta para representar una entidad de negocio
abstract class Entidad {
    Long id
    String nombre

    // Definir un método abstrato para obtener el nombre de la entidad
    abstract String getNombre()
}

// Definir una clase concreta que extienda de la clase abstracta Entidad
class Persona extends Entidad {

    // Definir propiedades adicionales para la clase Persona
    String apellidos
    Integer edad

    // Sobreescribir el método getNombre() para devolver el nombre completo de la persona
    @Override
    String getNombre() {
        return nombre + " " + apellidos
    }
}

// Definir una clase concreta que extienda de la clase abstracta Entidad
class Empresa extends Entidad {

    // Definir propiedades adicionales para la clase Empresa
    String razonSocial
    String nif

    // Sobreescribir el método getNombre() para devolver el nombre de la empresa
    @Override
    String getNombre() {
        return razonSocial
    }
}

// Definir una clase para representar una relación entre dos entidades
class Relacion {
    Entidad entidad1
    Entidad entidad2
    String tipoRelacion

    // Definir un método para obtener el tipo de relación
    String getTipoRelacion() {
        return tipoRelacion
    }
}

// Definir una clase para representar un conjunto de entidades
class ConjuntoEntidades {
    List<Entidad> entidades

    // Definir un método para añadir una entidad al conjunto
    void añadirEntidad(Entidad entidad) {
        entidades.add(entidad)
    }

    // Definir un método para obtener la lista de entidades del conjunto
    List<Entidad> getEntidades() {
        return entidades
    }
}

// Definir una clase para representar un conjunto de relaciones
class ConjuntoRelaciones {
    List<Relacion> relaciones

    // Definir un método para añadir una relación al conjunto
    void añadirRelacion(Relacion relacion) {
        relaciones.add(relacion)
    }

    // Definir un método para obtener la lista de relaciones del conjunto
    List<Relacion> getRelaciones() {
        return relaciones
    }
}

// Crear una instancia de la clase Persona
Persona persona1 = new Persona(nombre: "Juan", apellidos: "García", edad: 30)

// Crear una instancia de la clase Empresa
Empresa empresa1 = new Empresa(razonSocial: "Acme Corp.", nif: "123456789")

// Crear una instancia de la clase Relacion
Relacion relacion1 = new Relacion(entidad1: persona1, entidad2: empresa1, tipoRelacion: "Empleado")

// Crear una instancia de la clase ConjuntoEntidades
ConjuntoEntidades conjuntoEntidades = new ConjuntoEntidades()

// Añadir la persona y la empresa al conjunto de entidades
conjuntoEntidades.añadirEntidad(persona1)
conjuntoEntidades.añadirEntidad(empresa1)

// Crear una instancia de la clase ConjuntoRelaciones
ConjuntoRelaciones conjuntoRelaciones = new ConjuntoRelaciones()

// Añadir la relación al conjunto de relaciones
conjuntoRelaciones.añadirRelacion(relacion1)

// Imprimir el nombre de la persona y el nombre de la empresa de la primera entrada
println "Nombre de la persona: ${conjuntoEntidades.entidades[0].nombre}"
println "Nombre de la empresa: ${conjuntoEntidades.entidades[1].nombre}"

// Imprimir el tipo de relación de la primera entrada
println "Tipo de relación: ${conjuntoRelaciones.relaciones[0].tipoRelacion}"