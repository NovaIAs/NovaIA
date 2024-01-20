```smalltalk
~~ Clase Persona ~~

Objeto: Persona
   nombre
   edad
   sexo

Métodos:

   inicializar(_nombre: _edad: _sexo:)
      nombre := _nombre.
      edad := _edad.
      sexo := _sexo.

   nombre
      ^nombre.

   edad
      ^edad.

   sexo
      ^sexo.

   sePresenta
      ^ (nombre, ' tiene ', edad, ' años y es ', sexo).

~~ Clase Empleado ~~

Objeto: Empleado
   nombre
   edad
   sexo
   sueldo

Métodos:

   inicializar(_nombre: _edad: _sexo: _sueldo:)
      inicializar(_nombre: _edad: _sexo:).
      sueldo := _sueldo.

   sueldo
      ^sueldo.

   sePresenta
      ^ super sePresenta, ' y gano ', sueldo.

~~ Clase Ingeniero ~~

Objeto: Ingeniero
   nombre
   edad
   sexo
   sueldo
   especialidad

Métodos:

   inicializar(_nombre: _edad: _sexo: _sueldo: _especialidad:)
      inicializar(_nombre: _edad: _sexo: _sueldo:).
      especialidad := _especialidad.

   especialidad
      ^especialidad.

   sePresenta
      ^ super sePresenta, ' y soy ', especialidad.

~~ Ejemplo ~~

unEmp = Empleado nuevo inicializar('Juan', 30, 'masculino', 1000).
unIng = Ingeniero nuevo inicializar('Maria', 35, 'femenino', 2000, 'software').

unEmp sePresenta.
unIng sePresenta.
```

Explicación:

* Clase `Persona`: Define la clase base `Persona` con atributos como `nombre`, `edad` y `sexo`.

* Clase `Empleado`: Hereda de la clase `Persona` y agrega el atributo `sueldo`.

* Clase `Ingeniero`: Hereda de la clase `Empleado` y agrega el atributo `especialidad`.

* Método de clase `inicializar(_nombre: _edad: _sexo: [,_sueldo: ,_especialidad: ]):` Se llama cada vez que se crea un objeto de la clase. Inicializa los atributos del objeto. Los parámetros entre corchetes son opcionales en las clases heredadas.

* Métodos getters: Define métodos `nombre`, `edad`, `sexo` y `sueldo` para acceder a los atributos correspondientes.

* Método de instancia `sePresenta`: Define un método que devuelve una cadena con información sobre la persona, incluida su edad y sexo. La clase `Ingeniero` anula este método para incluir también su especialidad.

* Ejemplo: Crea objetos de las clases `Empleado` e `Ingeniero` y muestra la información sobre cada uno.