```smalltalk
"Definir una clase llamada 'Persona' con propiedades como 'nombre', 'apellido' y 'edad'."

Persona clase [
    nombre: '',
    apellido: '',
    edad: 0
]

"Definir una clase llamada 'Empleado' que herede de 'Persona' y agregue propiedades como 'salario' y 'cargo'."

Empleado clase [
    superPersona,
    salario: 0.0,
    cargo: ''
]

"Definir una clase llamada 'Cliente' que herede de 'Persona' y agregue una propiedad llamada 'compras'."

Cliente clase [
    superPersona,
    compras: Collection new
]

"Crear un ejemplo de una instancia de la clase 'Persona' llamada 'juan'."

juan := Persona new.
juan nombre: 'Juan'.
juan apellido: 'Pérez'.
juan edad: 30.

"Crear un ejemplo de una instancia de la clase 'Empleado' llamada 'maria'."

maria := Empleado new.
maria nombre: 'María'.
maria apellido: 'Gómez'.
maria edad: 25.
maria salario: 1000.0.
maria cargo: 'Ingeniera'.

"Crear un ejemplo de una instancia de la clase 'Cliente' llamada 'pedro'."

pedro := Cliente new.
pedro nombre: 'Pedro'.
pedro apellido: 'López'.
pedro edad: 40.
pedro compras: Collection with: 'Libro' with: 'CD'.

"Imprimir información sobre los objetos creados."

Transcript show: 'Información sobre la persona "juan": '; newline.
Transcript show: 'Nombre: ', juan nombre; newline.
Transcript show: 'Apellido: ', juan apellido; newline.
Transcript show: 'Edad: ', juan edad; newline.
Transcript show: newline.

Transcript show: 'Información sobre la empleada "maria": '; newline.
Transcript show: 'Nombre: ', maria nombre; newline.
Transcript show: 'Apellido: ', maria apellido; newline.
Transcript show: 'Edad: ', maria edad; newline.
Transcript show: 'Salario: ', maria salario; newline.
Transcript show: 'Cargo: ', maria cargo; newline.
Transcript show: newline.

Transcript show: 'Información sobre el cliente "pedro": '; newline.
Transcript show: 'Nombre: ', pedro nombre; newline.
Transcript show: 'Apellido: ', pedro apellido; newline.
Transcript show: 'Edad: ', pedro edad; newline.
Transcript show: 'Compras: ', pedro compras asString; newline.
```

Explicación:

* La primera parte del código define tres clases: `Persona`, `Empleado` y `Cliente`. La clase `Persona` es la clase base y las clases `Empleado` y `Cliente` heredan de ella. Cada clase tiene sus propias propiedades definidas, además de las heredadas de la clase base.
* La segunda parte del código crea objetos de las clases definidas anteriormente. Se crea un objeto `Persona` llamado `juan`, un objeto `Empleado` llamado `maria` y un objeto `Cliente` llamado `pedro`. Se establecen los valores de las propiedades de cada objeto.
* La tercera parte del código imprime información sobre los objetos creados en la ventana de salida.