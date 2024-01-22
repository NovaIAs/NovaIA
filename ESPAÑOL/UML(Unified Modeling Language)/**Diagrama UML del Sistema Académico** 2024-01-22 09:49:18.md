```
```

```
Diagrama de Clases:
- Clase Persona:
    - Atributos:
        - nombre: String
        - edad: int
        - sexo: String
- Clase Estudiante:
    - Atributos:
        - matricula: String
        - carrera: String
        - semestre: int
- Clase Profesor:
    - Atributos:
        - departamento: String
        - asignatura: String
        - horario: String
- Clase Curso:
    - Atributos:
        - nombre: String
        - codigo: String
        - creditos: int
- Clase Aula:
    - Atributos:
        - numero: String
        - capacidad: int
        - tipo: String

Diagrama de Componentes:
- Componente Sistema Academico:
    - Subcomponentes:
        - Componente Estudiantes
        - Componente Profesores
        - Componente Cursos
        - Componente Aulas
    - Interfaz:
        - RegistroEstudiante()
        - RegistroProfesor()
        - RegistroCurso()
        - RegistroAula()

Diagrama de Despliegue:
- Servidor Central:
    - Componentes:
        - Componente Sistema Academico
        - Componente Base de Datos
    - Tecnología:
        - Linux
        - MySQL


Explicación:

El diagrama de clases define las clases y sus relaciones. 
- La clase Persona es la clase base para las clases Estudiante y Profesor. 
- La clase Estudiante tiene atributos como la matrícula, la carrera y el semestre. 
- La clase Profesor tiene atributos como el departamento, la asignatura y el horario. 
- Las clases Curso y Aula tienen atributos como el nombre, el código, los créditos y la capacidad, respectivamente.

El diagrama de componentes define los componentes del sistema académico y sus interacciones. 
- El componente Sistema Académico contiene los subcomponentes Estudiantes, Profesores, Cursos y Aulas. 
- El componente Base de Datos contiene los datos del sistema. 
- El componente Interfaz proporciona una interfaz de usuario para interactuar con el sistema.

El diagrama de despliegue define la arquitectura del sistema académico. 
- El servidor central contiene los componentes del sistema académico y la base de datos. 
- Los clientes son computadoras que se conectan al servidor central para utilizar el sistema académico.