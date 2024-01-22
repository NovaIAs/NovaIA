```smalltalk
**Clase Persona**

```smalltalk
Persona subclass: [Persona]
    instanceVariableNames: 'nombre apellido direccion telefono'

    classVariableNames: ''

    poolDictionaries: ''

    category: 'Ejemplo'
```

**Método nombreCompleto**

```smalltalk
Persona >> nombreCompleto
    ^ self nombre , ' ', self apellido
```

**Método dirección**

```smalltalk
Persona >> dirección
    ^ self direccion
```

**Método teléfono**

```smalltalk
Persona >> teléfono
    ^ self telefono
```

**Método imprimir**

```smalltalk
Persona >> imprimir
    Transcript show: 'Nombre: ', self nombreCompleto, !cr.
    Transcript show: 'Dirección: ', self direccion, !cr.
    Transcript show: 'Teléfono: ', self telefono, !cr.
```

**Instanciar una persona**

```smalltalk
persona := Persona new.
persona nombre: 'Juan'.
persona apellido: 'García'.
persona dirección: 'Calle Mayor, 123'.
persona teléfono: '123456789'.
```

**Imprimir los datos de la persona**

```smalltalk
persona imprimir.
```

**Resultado**

```smalltalk
Nombre: Juan García
Dirección: Calle Mayor, 123
Teléfono: 123456789
```

Este código crea una clase llamada `Persona` con tres variables de instancia: `nombre`, `apellido` y `dirección`. También crea un método llamado `nombreCompleto` que devuelve el nombre completo de la persona, y un método llamado `imprimir` que imprime los datos de la persona en la consola.

A continuación, se instancia una persona llamada `persona` y se le asignan los valores correspondientes a su nombre, apellido, dirección y teléfono. Por último, se llama al método `imprimir` para mostrar los datos de la persona en la consola.

El resultado es:

```
Nombre: Juan García
Dirección: Calle Mayor, 123
Teléfono: 123456789
```