```tcl
# Definición de la clase 'Persona'
class Persona {
    variable nombre ""
    variable edad 0
    variable ciudad ""

    # Método constructor
    constructor {nombre edad ciudad} {
        set nombre $nombre
        set edad $edad
        set ciudad $ciudad
    }

    # Método para obtener el nombre de la persona
    proc getNombre {} {
        return $nombre
    }

    # Método para obtener la edad de la persona
    proc getEdad {} {
        return $edad
    }

    # Método para obtener la ciudad de la persona
    proc getCiudad {} {
        return $ciudad
    }

    # Método para establecer el nombre de la persona
    proc setNombre {nombre} {
        set nombre $nombre
    }

    # Método para establecer la edad de la persona
    proc setEdad {edad} {
        set edad $edad
    }

    # Método para establecer la ciudad de la persona
    proc setCiudad {ciudad} {
        set ciudad $ciudad
    }

    # Método para imprimir la información de la persona
    proc imprimir {} {
        puts "Nombre: $nombre"
        puts "Edad: $edad"
        puts "Ciudad: $ciudad"
    }
}

# Crear una instancia de la clase 'Persona'
persona1 = new Persona "Juan" 25 "Madrid"

# Imprimir la información de la persona
persona1 imprimir

# Cambiar el nombre de la persona
persona1 setNombre "María"

# Cambiar la edad de la persona
persona1 setEdad 30

# Cambiar la ciudad de la persona
persona1 setCiudad "Barcelona"

# Imprimir la información de la persona
persona1 imprimir
```

Explicación del código:

1. **Definición de la clase 'Persona':** Se define la clase 'Persona' utilizando la palabra clave 'class'. La clase contiene tres variables de instancia: 'nombre', 'edad' y 'ciudad'. También contiene siete métodos: un constructor, tres métodos 'get' para obtener el nombre, la edad y la ciudad, tres métodos 'set' para establecer el nombre, la edad y la ciudad, y un método 'imprimir' para imprimir la información de la persona.

2. **Creación de una instancia de la clase 'Persona':** Se utiliza la palabra clave 'new' para crear una instancia de la clase 'Persona'. Se pasan tres argumentos al constructor: "Juan", 25 y "Madrid". Estos argumentos se asignan a las variables de instancia 'nombre', 'edad' y 'ciudad', respectivamente.

3. **Impresión de la información de la persona:** Se llama al método 'imprimir' de la instancia 'persona1' para imprimir la información de la persona. El método 'imprimir' utiliza la instrucción 'puts' para imprimir el nombre, la edad y la ciudad de la persona.

4. **Cambio del nombre de la persona:** Se llama al método 'setNombre' de la instancia 'persona1' para cambiar el nombre de la persona. Se pasa el argumento "María" al método 'setNombre'. Este argumento se asigna a la variable de instancia 'nombre'.

5. **Cambio de la edad de la persona:** Se llama al método 'setEdad' de la instancia 'persona1' para cambiar la edad de la persona. Se pasa el argumento 30 al método 'setEdad'. Este argumento se asigna a la variable de instancia 'edad'.

6. **Cambio de la ciudad de la persona:** Se llama al método 'setCiudad' de la instancia 'persona1' para cambiar la ciudad de la persona. Se pasa el argumento "Barcelona" al método 'setCiudad'. Este argumento se asigna a la variable de instancia 'ciudad'.

7. **Impresión de la información de la persona:** Se llama de nuevo al método 'imprimir' de la instancia 'persona1' para imprimir la información de la persona. El método 'imprimir' utiliza la instrucción 'puts' para imprimir el nombre, la edad y la ciudad de la persona.