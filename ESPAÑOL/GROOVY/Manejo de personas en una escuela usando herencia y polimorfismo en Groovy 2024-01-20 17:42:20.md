```groovy
// Definimos una clase llamada "Persona" con sus propiedades y métodos.
class Persona {

    // Propiedades de la clase.
    String nombre
    String apellido
    int edad

    // Método constructor de la clase.
    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Método para obtener el nombre completo de la persona.
    String getNombreCompleto() {
        return "$nombre $apellido"
    }

    // Método para obtener la edad de la persona.
    int getEdad() {
        return edad
    }

    // Método para saludar a la persona.
    String saludar() {
        return "Hola, mi nombre es $nombre $apellido y tengo $edad años."
    }
}

// Definimos una clase llamada "Estudiante" que hereda de la clase "Persona".
class Estudiante extends Persona {

    // Propiedades de la clase.
    String carrera
    int añoDeEstudio

    // Método constructor de la clase.
    Estudiante(String nombre, String apellido, int edad, String carrera, int añoDeEstudio) {
        super(nombre, apellido, edad)
        this.carrera = carrera
        this.añoDeEstudio = añoDeEstudio
    }

    // Método para obtener la carrera del estudiante.
    String getCarrera() {
        return carrera
    }

    // Método para obtener el año de estudio del estudiante.
    int getAñoDeEstudio() {
        return añoDeEstudio
    }

    // Método para saludar al estudiante.
    String saludar() {
        return "Hola, mi nombre es $nombre $apellido, tengo $edad años, estudio $carrera y estoy en el año $añoDeEstudio."
    }
}

// Definimos una clase llamada "Profesor" que hereda de la clase "Persona".
class Profesor extends Persona {

    // Propiedades de la clase.
    String materia
    int añosDeExperiencia

    // Método constructor de la clase.
    Profesor(String nombre, String apellido, int edad, String materia, int añosDeExperiencia) {
        super(nombre, apellido, edad)
        this.materia = materia
        this.añosDeExperiencia = añosDeExperiencia
    }

    // Método para obtener la materia del profesor.
    String getMateria() {
        return materia
    }

    // Método para obtener los años de experiencia del profesor.
    int getAñosDeExperiencia() {
        return añosDeExperiencia
    }

    // Método para saludar al profesor.
    String saludar() {
        return "Hola, mi nombre es $nombre $apellido, tengo $edad años, enseño $materia y tengo $añosDeExperiencia años de experiencia."
    }
}

// Definimos una clase llamada "Escuela" que contiene una lista de personas.
class Escuela {

    // Propiedad de la clase.
    List<Persona> personas

    // Método constructor de la clase.
    Escuela() {
        personas = new ArrayList<>()
    }

    // Método para agregar una persona a la lista.
    void agregarPersona(Persona persona) {
        personas.add(persona)
    }

    // Método para obtener la lista de personas.
    List<Persona> getPersonas() {
        return personas
    }

    // Método para saludar a todas las personas de la lista.
    String saludarATodos() {
        StringBuilder saludo = new StringBuilder()
        for (Persona persona : personas) {
            saludo.append(persona.saludar()).append("\n")
        }
        return saludo.toString()
    }
}

// Creamos una escuela y agregamos algunas personas a la lista.
Escuela escuela = new Escuela()
escuela.agregarPersona(new Estudiante("Juan", "Pérez", 20, "Ingeniería en Informática", 3))
escuela.agregarPersona(new Profesor("María", "Gómez", 40, "Matemáticas", 15))
escuela.agregarPersona(new Persona("Pedro", "Rodríguez", 30))

// Saludamos a todas las personas de la escuela.
println escuela.saludarATodos()
```

**Explicación:**

1. Definimos tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Estudiante` hereda de la clase `Persona`, y la clase `Profesor` también hereda de la clase `Persona`.
2. La clase `Persona` tiene las propiedades `nombre`, `apellido` y `edad`, y los métodos `getNombreCompleto()`, `getEdad()` y `saludar()`.
3. La clase `Estudiante` tiene las propiedades `carrera` y `añoDeEstudio`, y los métodos `getCarrera()`, `getAñoDeEstudio()` y `saludar()`.
4. La clase `Profesor` tiene las propiedades `materia` y `añosDeExperiencia`, y los métodos `getMateria()`, `getAñosDeExperiencia()` y `saludar()`.
5. La clase `Escuela` tiene una propiedad `personas`, que es una lista de personas, y los métodos `agregarPersona()`, `getPersonas()` y `saludarATodos()`.
6. Creamos una escuela y agregamos algunas personas a la lista, utilizando los objetos de las clases `Estudiante`, `Profesor` y `Persona`.
7. Finalmente, saludamos a todas las personas de la escuela utilizando el método `saludarATodos()`.

Este código es complejo porque utiliza herencia, polimorfismo y colecciones. También es amplio y diferenciado porque tiene muchas clases, métodos y propiedades, y porque cubre una amplia gama de funcionalidades.