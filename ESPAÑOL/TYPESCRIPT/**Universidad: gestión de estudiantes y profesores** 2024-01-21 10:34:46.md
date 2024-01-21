```typescript
// Definición de la clase Persona
export class Persona {
    nombre: string;
    apellido: string;
    edad: number;

    constructor(nombre: string, apellido: string, edad: number) {
        this.nombre = nombre;
        this.apellido = apellido;
        this.edad = edad;
    }

    // Método para obtener el nombre completo de la persona
    getNombreCompleto(): string {
        return `${this.nombre} ${this.apellido}`;
    }

    // Método para obtener la edad de la persona en años
    getEdadEnAnos(): number {
        return this.edad;
    }
}

// Definición de la clase Estudiante
export class Estudiante extends Persona {
    carrera: string;
    promedio: number;

    constructor(nombre: string, apellido: string, edad: number, carrera: string, promedio: number) {
        super(nombre, apellido, edad);
        this.carrera = carrera;
        this.promedio = promedio;
    }

    // Método para obtener la carrera del estudiante
    getCarrera(): string {
        return this.carrera;
    }

    // Método para obtener el promedio del estudiante
    getPromedio(): number {
        return this.promedio;
    }
}

// Definición de la clase Profesor
export class Profesor extends Persona {
    asignatura: string;
    salario: number;

    constructor(nombre: string, apellido: string, edad: number, asignatura: string, salario: number) {
        super(nombre, apellido, edad);
        this.asignatura = asignatura;
        this.salario = salario;
    }

    // Método para obtener la asignatura del profesor
    getAsignatura(): string {
        return this.asignatura;
    }

    // Método para obtener el salario del profesor
    getSalario(): number {
        return this.salario;
    }
}

// Definición de la clase Universidad
export class Universidad {
    nombre: string;
    direccion: string;
    estudiantes: Estudiante[];
    profesores: Profesor[];

    constructor(nombre: string, direccion: string, estudiantes: Estudiante[], profesores: Profesor[]) {
        this.nombre = nombre;
        this.direccion = direccion;
        this.estudiantes = estudiantes;
        this.profesores = profesores;
    }

    // Método para obtener el nombre de la universidad
    getNombre(): string {
        return this.nombre;
    }

    // Método para obtener la dirección de la universidad
    getDireccion(): string {
        return this.direccion;
    }

    // Método para obtener la lista de estudiantes de la universidad
    getEstudiantes(): Estudiante[] {
        return this.estudiantes;
    }

    // Método para obtener la lista de profesores de la universidad
    getProfesores(): Profesor[] {
        return this.profesores;
    }
}

// Creación de la universidad
const universidad = new Universidad('Universidad de Salamanca', 'Calle del Patio de Escuelas, 1, 37008 Salamanca', [], []);

// Creación de los estudiantes
const estudiante1 = new Estudiante('Juan', 'García', 20, 'Ingeniería Informática', 8.5);
const estudiante2 = new Estudiante('María', 'López', 21, 'Administración y Dirección de Empresas', 9.0);
const estudiante3 = new Estudiante('Pedro', 'Sánchez', 22, 'Derecho', 7.5);

// Creación de los profesores
const profesor1 = new Profesor('Antonio', 'García', 50, 'Matemáticas', 2000);
const profesor2 = new Profesor('María', 'López', 45, 'Física', 2500);
const profesor3 = new Profesor('Pedro', 'Sánchez', 40, 'Química', 3000);

// Adición de los estudiantes y profesores a la universidad
universidad.estudiantes.push(estudiante1);
universidad.estudiantes.push(estudiante2);
universidad.estudiantes.push(estudiante3);
universidad.profesores.push(profesor1);
universidad.profesores.push(profesor2);
universidad.profesores.push(profesor3);

// Impresión de la información de la universidad
console.log(`Nombre de la universidad: ${universidad.getNombre()}`);
console.log(`Dirección de la universidad: ${universidad.getDireccion()}`);
console.log('Estudiantes de la universidad:');
universidad.estudiantes.forEach((estudiante) => {
    console.log(`Nombre: ${estudiante.getNombreCompleto()}`);
    console.log(`Carrera: ${estudiante.getCarrera()}`);
    console.log(`Promedio: ${estudiante.getPromedio()}`);
});
console.log('Profesores de la universidad:');
universidad.profesores.forEach((profesor) => {
    console.log(`Nombre: ${profesor.getNombreCompleto()}`);
    console.log(`Asignatura: ${profesor.getAsignatura()}`);
    console.log(`Salario: ${profesor.getSalario()}`);
});
```

Explicaciones adicionales:

1. **Clase Persona**: Esta clase representa a una persona genérica. Tiene atributos para el nombre, apellido y la edad. También tiene un método para obtener el nombre completo y otro para obtener la edad en años.


2. **Clase Estudiante**: Esta clase hereda de la clase Persona y representa a un estudiante. Tiene atributos para la carrera y el promedio. También tiene métodos para obtener la carrera y el promedio.


3. **Clase Profesor**: Esta clase también hereda de la clase Persona y representa a un profesor. Tiene atributos para la asignatura y el salario. También tiene métodos para obtener la asignatura y el salario.


4. **Clase Universidad**: Esta clase representa a una universidad. Tiene atributos para el nombre, la dirección, los estudiantes y los profesores. También tiene métodos para obtener toda esta información.


5. **Creación de la universidad, los estudiantes y los profesores**: Se crean los objetos de las clases Universidad, Estudiante y Profesor, y se añaden a la universidad.


6. **Impresión de la información de la universidad**: Se imprimen el nombre, la dirección, los estudiantes y los profesores de la universidad.