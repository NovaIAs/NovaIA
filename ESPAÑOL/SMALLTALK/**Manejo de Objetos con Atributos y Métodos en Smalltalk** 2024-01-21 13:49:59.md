```smalltalk
"Definir una clase llamada 'Persona'."
Persona clase

"Definir atributos para la clase 'Persona'."
atributos: {
    nombre,
    edad,
    sexo
}

"Definir métodos para la clase 'Persona'."
métodos: {

    "Constructor de la clase 'Persona'."
    iniciarConNombre: unNombre edad: unaEdad sexo: unSexo
        "Crear una nueva instancia de la clase 'Persona' con los valores especificados para los atributos."
        nombre := unNombre.
        edad := unaEdad.
        sexo := unSexo.
    fin.  

    "Obtener el nombre de la persona."
    nombre
        "Devolver el valor del atributo 'nombre'."
        ^nombre
    fin.

    "Establecer el nombre de la persona."
    nombre: unNombre
        "Establecer el valor del atributo 'nombre'."
        nombre := unNombre
    fin.  

    "Obtener la edad de la persona."
    edad
        "Devolver el valor del atributo 'edad'."
        ^edad
    fin.

    "Establecer la edad de la persona."
    edad: unaEdad
        "Establecer el valor del atributo 'edad'."
        edad := unaEdad
    fin.  

    "Obtener el sexo de la persona."
    sexo
        "Devolver el valor del atributo 'sexo'."
        ^sexo
    fin.

    "Establecer el sexo de la persona."
    sexo: unSexo
        "Establecer el valor del atributo 'sexo'."
        sexo := unSexo
    fin.

    "Mostrar información de la persona."
    mostrar
        "Imprimir la información de la persona en la salida estándar."
        Transcript show: 'Nombre: ', nombre.
        Transcript show: ', Edad: ', edad.
        Transcript show: ', Sexo: ', sexo.
    fin.
}
fin.


"Crear una instancia de la clase 'Persona'."
unaPersona := Persona iniciarConNombre: 'Juan' edad: 25 sexo: 'Masculino'.


"Mostrar información de la persona."
unaPersona mostrar.


"Cambiar el nombre de la persona."
unaPersona nombre: 'Pedro'.


"Mostrar información de la persona."
unaPersona mostrar.
```

Explicación del código:

* **Definir una clase llamada 'Persona':** Esta línea define una nueva clase llamada 'Persona'.


* **Definir atributos para la clase 'Persona':** Esta línea define los atributos de la clase 'Persona'. En este caso, los atributos son 'nombre', 'edad' y 'sexo'.


* **Definir métodos para la clase 'Persona':** Esta línea define los métodos de la clase 'Persona'. En este caso, los métodos son 'iniciarConNombre:edad:sexo:', 'nombre', 'nombre:', 'edad', 'edad:', 'sexo', 'sexo:', 'mostrar'.


* **Constructor de la clase 'Persona':** Este método es el constructor de la clase 'Persona'. Se utiliza para crear nuevas instancias de la clase 'Persona'. El método toma tres argumentos: 'unNombre', 'unaEdad' y 'unSexo'. Estos argumentos se utilizan para inicializar los atributos de la nueva instancia.


* **Obtener el nombre de la persona:** Este método obtiene el valor del atributo 'nombre' de la persona.


* **Establecer el nombre de la persona:** Este método establece el valor del atributo 'nombre' de la persona.


* **Obtener la edad de la persona:** Este método obtiene el valor del atributo 'edad' de la persona.


* **Establecer la edad de la persona:** Este método establece el valor del atributo 'edad' de la persona.


* **Obtener el sexo de la persona:** Este método obtiene el valor del atributo 'sexo' de la persona.


* **Establecer el sexo de la persona:** Este método establece el valor del atributo 'sexo' de la persona.


* **Mostrar información de la persona:** Este método muestra información de la persona en la salida estándar.

* **Crear una instancia de la clase 'Persona':** Esta línea crea una nueva instancia de la clase 'Persona' utilizando el constructor 'Persona iniciarConNombre:edad:sexo:'. Los argumentos pasados al constructor son 'Juan', '25' y 'Masculino'.


* **Mostrar información de la persona:** Esta línea llama al método 'mostrar' de la instancia 'unaPersona' para mostrar información de la persona en la salida estándar.


* **Cambiar el nombre de la persona:** Esta línea llama al método 'nombre:' de la instancia 'unaPersona' para cambiar el nombre de la persona. El argumento pasado al método es 'Pedro'.


* **Mostrar información de la persona:** Esta línea vuelve a llamar al método 'mostrar' de la instancia 'unaPersona' para mostrar información de la persona en la salida estándar.