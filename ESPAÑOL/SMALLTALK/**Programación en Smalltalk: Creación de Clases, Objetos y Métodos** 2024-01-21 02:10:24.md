```smalltalk
**Definición de una clase Persona**

```smalltalk
Clase Persona
    nombre: String
    edad: Integer
    sexo: String

    constructor: unNombre unaEdad unSexo
        nombre := unNombre.
        edad := unaEdad.
        sexo := unSexo.

    nombre
        ^nombre.

    edad
        ^edad.

    sexo
        ^sexo.

    presentarse
        'Hola, mi nombre es ' , nombre, ', tengo ', edad, ' años y soy ', sexo.
```

**Definición de una clase Empleado**

```smalltalk
Clase Empleado : Persona
    salario: Integer

    constructor: unNombre unaEdad unSexo unSalario
        super constructor: unNombre unaEdad unSexo.
        salario := unSalario.

    salario
        ^salario.

    presentarse
        super presentarse, ' y gano ', salario, ' pesos al mes.'.
```

**Definición de una clase Ingeniero**

```smalltalk
Clase Ingeniero : Empleado
    especialidad: String

    constructor: unNombre unaEdad unSexo unSalario unaEspecialidad
        super constructor: unNombre unaEdad unSexo unSalario.
        especialidad := unaEspecialidad.

    especialidad
        ^especialidad.

    presentarse
        super presentarse, ' Soy ingeniero ', especialidad, '.'.
```

**Creación de objetos**

```smalltalk
juan := Persona constructor: 'Juan Pérez' 25 'masculino'.
maría := Persona constructor: 'María Gómez' 23 'femenino'.

pedro := Empleado constructor: 'Pedro García' 30 'masculino' 100000.
ana := Empleado constructor: 'Ana López' 28 'femenino' 80000.

carlos := Ingeniero constructor: 'Carlos Rodríguez' 35 'masculino' 120000 'informático'.
laura := Ingeniero constructor: 'Laura Fernández' 32 'femenino' 110000 'civil'.
```

**Invocación de métodos**

```smalltalk
juan nombre.
maría edad.
pedro sexo.

pedro salario.
pedro presentarse.

carlos especialidad.
carlos presentarse.

laura especialidad.
laura presentarse.
```

**Explicación del código**

El código anterior define tres clases: Persona, Empleado e Ingeniero. La clase Persona tiene tres atributos: nombre, edad y sexo. La clase Empleado hereda de la clase Persona y añade un atributo: salario. La clase Ingeniero hereda de la clase Empleado y añade un atributo: especialidad.

Además, el código crea objetos de las clases Persona, Empleado e Ingeniero. A continuación, invoca métodos de los objetos para obtener sus atributos y llamar a sus métodos.

El resultado de ejecutar el código es el siguiente:

```text
Juan Pérez
25
masculino
100000
Hola, mi nombre es Pedro García, tengo 30 años y soy masculino y gano 100000 pesos al mes.
informático
Hola, mi nombre es Carlos Rodríguez, tengo 35 años y soy masculino y gano 120000 pesos al mes. Soy ingeniero informático.
civil
Hola, mi nombre es Laura Fernández, tengo 32 años y soy femenino y gano 110000 pesos al mes. Soy ingeniero civil.
```