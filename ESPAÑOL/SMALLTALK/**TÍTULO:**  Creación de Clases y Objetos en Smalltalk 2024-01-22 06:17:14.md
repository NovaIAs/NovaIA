```smalltalk
```
**Español:**

Crear una clase con el nombre `Estudiante`.

```smalltalk
Object subclass: Estudiante
```

Añadir atributos a la clase `Estudiante`.

```smalltalk
Estudiante class addAttribute: 'nombre'
Estudiante class addAttribute: 'apellido'
Estudiante class addAttribute: 'matricula'
```

Crear un método para obtener el nombre completo de un estudiante.

```smalltalk
Estudiante class addMethod: 'nombreCompleto' arguments: nil with:
    ^ [self nombre] asString & ' ' & [self apellido] asString
```

Crear un método para cambiar el nombre completo de un estudiante.

```smalltalk
Estudiante class addMethod: 'cambiarNombre:' arguments: 'nuevoNombre' with:
    [self nombre: (nuevoNombre asString trimRight)]
    [self apellido: (nuevoNombre asString trimLeft)]
```

Crear un método para obtener la matrícula de un estudiante.

```smalltalk
Estudiante class addMethod: 'matricula' arguments: nil with:
    ^ self matricula
```

Crear un método para cambiar la matrícula de un estudiante.

```smalltalk
Estudiante class addMethod: 'matricula:' arguments: 'nuevaMatricula' with:
    self matricula: nuevaMatricula
```

Crear un método para imprimir la información de un estudiante.

```smalltalk
Estudiante class addMethod: 'imprimir' arguments: nil with:
    Transcript show: ('Nombre: ' & [self nombreCompleto] asString) cr.
    Transcript show: ('Matrícula: ' & [self matricula] asString) cr
```

Crear un objeto de la clase `Estudiante`.

```smalltalk
estudiante := Estudiante new.
```

Establecer el nombre, apellido y matrícula del estudiante.

```smalltalk
estudiante nombre: 'Juan'.
estudiante apellido: 'Pérez'.
estudiante matricula: '123456'.
```

Imprimir la información del estudiante.

```smalltalk
estudiante imprimir.
```

**Resultado:**

```
Nombre: Juan Pérez
Matrícula: 123456
```

**Explicación:**

En este código, hemos creado una clase llamada `Estudiante` que representa a un estudiante con un nombre, apellido y matrícula. Hemos añadido métodos para obtener y cambiar el nombre completo, la matrícula y para imprimir la información del estudiante. Luego, hemos creado un objeto de la clase `Estudiante`, hemos establecido su nombre, apellido y matrícula, y hemos impreso su información.

Este código muestra cómo crear clases y objetos en Smalltalk, así como cómo añadir atributos y métodos a las clases.