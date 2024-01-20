```javascript
// Definición de clase Persona
class Persona {
    constructor(nombre, edad, ocupacion) {
        this.nombre = nombre;
        this.edad = edad;
        this.ocupacion = ocupacion;
    }

    hablar() {
        console.log(`Hola, soy ${this.nombre} y tengo ${this.edad} años. Soy ${this.ocupacion}.`);
    }
}

// Definición de clase Estudiante, que hereda de Persona
class Estudiante extends Persona {
    constructor(nombre, edad, ocupacion, curso) {
        super(nombre, edad, ocupacion);
        this.curso = curso;
    }

    estudiar() {
        console.log(`${this.nombre} está estudiando ${this.curso}.`);
    }
}

// Definición de clase Profesor, que hereda de Persona
class Profesor extends Persona {
    constructor(nombre, edad, ocupacion, materia) {
        super(nombre, edad, ocupacion);
        this.materia = materia;
    }

    enseñar() {
        console.log(`${this.nombre} está enseñando ${this.materia}.`);
    }
}

// Creación de objetos Persona, Estudiante y Profesor
const persona1 = new Persona('Juan', 25, 'Ingeniero');
const estudiante1 = new Estudiante('María', 20, 'Estudiante', 'Ingeniería Informática');
const profesor1 = new Profesor('Pedro', 40, 'Profesor', 'Matemáticas');

// Llamada a los métodos de los objetos
persona1.hablar();
estudiante1.hablar();
estudiante1.estudiar();
profesor1.hablar();
profesor1.enseñar();
```

Este código define tres clases: Persona, Estudiante y Profesor. La clase Persona es la clase base, y las clases Estudiante y Profesor heredan de ella.

```javascript
class Persona {
    constructor(nombre, edad, ocupacion) {
        this.nombre = nombre;
        this.edad = edad;
        this.ocupacion = ocupacion;
    }

    hablar() {
        console.log(`Hola, soy ${this.nombre} y tengo ${this.edad} años. Soy ${this.ocupacion}.`);
    }
}
```

La clase Persona tiene un constructor que toma tres parámetros: nombre, edad y ocupación. También tiene un método llamado hablar() que imprime un mensaje con el nombre, la edad y la ocupación de la persona.

```javascript
class Estudiante extends Persona {
    constructor(nombre, edad, ocupacion, curso) {
        super(nombre, edad, ocupacion);
        this.curso = curso;
    }

    estudiar() {
        console.log(`${this.nombre} está estudiando ${this.curso}.`);
    }
}
```

La clase Estudiante hereda de la clase Persona. Tiene un constructor que toma cuatro parámetros: nombre, edad, ocupación y curso. También tiene un método llamado estudiar() que imprime un mensaje con el nombre del estudiante y el curso que está estudiando.

```javascript
class Profesor extends Persona {
    constructor(nombre, edad, ocupacion, materia) {
        super(nombre, edad, ocupacion);
        this.materia = materia;
    }

    enseñar() {
        console.log(`${this.nombre} está enseñando ${this.materia}.`);
    }
}
```

La clase Profesor hereda de la clase Persona. Tiene un constructor que toma cuatro parámetros: nombre, edad, ocupación y materia. También tiene un método llamado enseñar() que imprime un mensaje con el nombre del profesor y la materia que está enseñando.

```javascript
const persona1 = new Persona('Juan', 25, 'Ingeniero');
const estudiante1 = new Estudiante('María', 20, 'Estudiante', 'Ingeniería Informática');
const profesor1 = new Profesor('Pedro', 40, 'Profesor', 'Matemáticas');
```

Este código crea tres objetos: una persona llamada Juan, una estudiante llamada María y un profesor llamado Pedro.

```javascript
persona1.hablar();
estudiante1.hablar();
estudiante1.estudiar();
profesor1.hablar();
profesor1.enseñar();
```

Este código llama a los métodos hablar() y estudiar() de los objetos persona1 y estudiante1, respectivamente. También llama a los métodos hablar() y enseñar() del objeto profesor1.