```c#
// Definición de clases
class Persona {
    public string nombre;
    public int edad;

    public Persona(string nombre, int edad) {
        this.nombre = nombre;
        this.edad = edad;
    }

    public void Saludar() {
        Console.WriteLine($"¡Hola! Soy {nombre} y tengo {edad} años.");
    }
}

class Estudiante : Persona {
    public string carrera;
    public int semestre;

    public Estudiante(string nombre, int edad, string carrera, int semestre)
        : base(nombre, edad) {
        this.carrera = carrera;
        this.semestre = semestre;
    }

    public override void Saludar() {
        Console.WriteLine($"¡Hola! Soy {nombre} y soy estudiante de {carrera} en el semestre {semestre}.");
    }
}

class Profesor : Persona {
    public string materia;
    public int añosDeServicio;

    public Profesor(string nombre, int edad, string materia, int añosDeServicio)
        : base(nombre, edad) {
        this.materia = materia;
        this.añosDeServicio = añosDeServicio;
    }

    public override void Saludar() {
        Console.WriteLine($"¡Hola! Soy {nombre} y soy profesor de {materia} con {añosDeServicio} años de servicio.");
    }
}

// Creación de objetos
Persona persona1 = new Persona("Juan", 25);
Estudiante estudiante1 = new Estudiante("María", 20, "Ingeniería en Sistemas", 5);
Profesor profesor1 = new Profesor("Pedro", 40, "Matemáticas", 15);

// Invocación de métodos
persona1.Saludar();
estudiante1.Saludar();
profesor1.Saludar();
```

Explicación del código:

* Se definen tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` es la clase base de las otras dos clases.
* La clase `Persona` tiene dos propiedades: `nombre` y `edad`. La clase `Estudiante` tiene dos propiedades adicionales: `carrera` y `semestre`. La clase `Profesor` tiene dos propiedades adicionales: `materia` y `añosDeServicio`.
* La clase `Persona` tiene un método llamado `Saludar()`. La clase `Estudiante` y la clase `Profesor` tienen un método `Saludar()` sobreescrito que imprime un mensaje diferente.
* Se crean tres objetos: un objeto `Persona`, un objeto `Estudiante` y un objeto `Profesor`.
* Se invocan los métodos `Saludar()` de los tres objetos, lo que imprime los mensajes correspondientes.