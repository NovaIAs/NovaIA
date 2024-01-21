```smalltalk
(Definimos una clase llamada "Persona" con atributos "nombre", "edad" y "sexo".)
Persona := clase [
    nombre: '',
    edad: 0,
    sexo: ''
].

(Definimos una clase llamada "Estudiante" que hereda de "Persona" y añade el atributo "calificaciones".)
Estudiante := Persona subclass [
    calificaciones: {}
].

(Definimos una clase llamada "Profesor" que hereda de "Persona" y añade los atributos "materias" y "salario".)
Profesor := Persona subclass [
    materias: {},
    salario: 0
].

(Instanciamos algunos objetos de las clases "Persona", "Estudiante" y "Profesor".)
juan := Persona new.
maria := Estudiante new.
jose := Profesor new.

(Establecemos los valores de los atributos de los objetos.)
juan nombre: 'Juan'.
juan edad: 30.
juan sexo: 'Masculino'.

maria nombre: 'María'.
maria edad: 20.
maria sexo: 'Femenino'.
maria calificaciones: { 'Matemáticas': 9, 'Física': 8, 'Química': 7 }.

jose nombre: 'José'.
jose edad: 40.
jose sexo: 'Masculino'.
jose materias: { 'Matemáticas', 'Física', 'Química' }.
jose salario: 3000.

(Imprimimos los valores de los atributos de los objetos.)
juan print.
maria print.
jose print.

(Definimos una clase llamada "Colegio" que tiene una lista de estudiantes y profesores.)
Colegio := clase [
    estudiantes: {},
    profesores: {}
].

(Instanciamos un objeto de la clase "Colegio".)
colegio := Colegio new.

(Añadimos los objetos "maria" y "jose" a la lista de estudiantes y profesores del colegio, respectivamente.)
colegio estudiantes add: maria.
colegio profesores add: jose.

(Imprimimos la lista de estudiantes y profesores del colegio.)
colegio estudiantes print.
colegio profesores print.
```

Explicación del código:

* Definimos tres clases: "Persona", "Estudiante" y "Profesor". La clase "Persona" tiene tres atributos: "nombre", "edad" y "sexo". La clase "Estudiante" hereda de la clase "Persona" y añade un atributo llamado "calificaciones". La clase "Profesor" también hereda de la clase "Persona" y añade dos atributos llamados "materias" y "salario".
* Instanciamos algunos objetos de las clases "Persona", "Estudiante" y "Profesor". Asignamos valores a los atributos de los objetos.
* Imprimimos los valores de los atributos de los objetos.
* Definimos una clase llamada "Colegio" que tiene una lista de estudiantes y profesores. Instanciamos un objeto de la clase "Colegio". Añadimos los objetos "maria" y "jose" a la lista de estudiantes y profesores del colegio, respectivamente.
* Imprimimos la lista de estudiantes y profesores del colegio.