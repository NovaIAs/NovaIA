**Diagrama de Clases:**

```uml
- Clase: Persona {
  + atributos:
    - nombre: String
    - apellido: String
    - edad: int
  + métodos:
    - Constructor(nombre: String, apellido: String, edad: int)
    - getNombre(): String
    - getApellido(): String
    - getEdad(): int
    - toString(): String
}

- Clase: Estudiante {
  + atributos:
    - matricula: int
    - promedio: float
  + métodos:
    - Constructor(nombre: String, apellido: String, edad: int, matricula: int, promedio: float)
    - getMatricula(): int
    - getPromedio(): float
    - toString(): String
}

- Clase: Profesor {
  + atributos:
    - departamento: String
    - salario: float
  + métodos:
    - Constructor(nombre: String, apellido: String, edad: int, departamento: String, salario: float)
    - getDepartamento(): String
    - getSalario(): float
    - toString(): String
}
```

**Diagrama de Objetos:**

```uml
- objeto: persona1 = new Persona("Juan", "Pérez", 20)
- objeto: estudiante1 = new Estudiante("María", "Fernández", 18, 1001, 9.0)
- objeto: profesor1 = new Profesor("José", "García", 40, "Ciencias", 2000.0)
```

**Diagrama de Secuencia:**

```uml
persona1 -> estudiante1: getMatricula()
estudiante1 -> persona1: setMatricula(1002)

persona1 -> profesor1: getSalario()
profesor1 -> persona1: setSalario(2500.0)
```

**Diagrama de Estado:**

```uml
- Estado: Nuevo
  - Transición: aprobar
    - Destino: En progreso

- Estado: En progreso
  - Transición: reprobar
    - Destino: Reprobado
  - Transición: aprobar
    - Destino: Aprobado
```

**Diagrama de Casos de Uso:**

```uml
- Caso de uso: Registrar estudiante
  - Actores:
    - Estudiante
    - Secretario académico
  - Flujo principal:
    1. El estudiante solicita su registro.
    2. El secretario académico verifica los requisitos.
    3. El secretario académico registra al estudiante en el sistema.
    4. El estudiante recibe su matrícula.

- Caso de uso: Calificar examen
  - Actores:
    - Profesor
    - Estudiante
  - Flujo principal:
    1. El profesor publica el examen en el sistema.
    2. Los estudiantes realizan el examen.
    3. El profesor califica los exámenes.
    4. Los estudiantes reciben sus calificaciones.
```

**Explicación del código:**

El código anterior es un ejemplo de un modelo UML completo para un sistema de gestión de estudiantes. El diagrama de clases define las clases del sistema, el diagrama de objetos crea objetos de esas clases, el diagrama de secuencia muestra cómo interactúan los objetos entre sí, el diagrama de estado muestra los estados por los que puede pasar un objeto, y el diagrama de casos de uso define los casos de uso del sistema.

Este código se puede utilizar para generar código fuente para un sistema de gestión de estudiantes. También se puede utilizar para documentar el sistema y para comunicar los requisitos del sistema a los desarrolladores.