```smalltalk
clase Jefatura {
    nombre: 'Jefatura de Departamento'
    miembros: Set new.
}

clase Departamento {
    nombre: 'Departamento de Informática'
    miembros: Set new.
    jefatura: Jefatura new.
}

clase Profesor {
    nombre: 'Juan García'
    departamento: Departamento new.
}

clase Alumno {
    nombre: 'María Sánchez'
    departamento: Departamento new.
}

clase Universidad {
    nombre: 'Universidad de Sevilla'
    decanatos: Set new.
    departamentos: Set new.
}

profesor := Profesor new.
alumno := Alumno new.
departamento := Departamento new.
universidad := Universidad new.

universidad addDepartamento: departamento.
departamento addMiembro: profesor.
departamento addMiembro: alumno.

(profesor departamento) printNl.
(alumno departamento) printNl.
(departamento universidad) printNl.
(universidad departamentos) printNl.
```

Explicación del código:

* Primero, se definen las clases necesarias para modelar la universidad: `Jefatura`, `Departamento`, `Profesor`, `Alumno` y `Universidad`.
* Luego, se crean objetos de cada clase y se establecen las relaciones entre ellos. Por ejemplo, el profesor y el alumno pertenecen al departamento, y el departamento pertenece a la universidad.
* Por último, se muestran por pantalla los resultados de las relaciones entre los objetos. Por ejemplo, se muestra el nombre del departamento al que pertenece el profesor, el alumno y el departamento.

Este código es un ejemplo de cómo usar programación orientada a objetos en Smalltalk para modelar una universidad.