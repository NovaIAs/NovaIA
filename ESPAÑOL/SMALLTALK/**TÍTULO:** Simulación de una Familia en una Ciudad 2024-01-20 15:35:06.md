```smalltalk

**Clase Persona**

Persona clase

| nombre | edad | ciudad |

instancia nueva
    ^ self
    new nombre: 'Juan' edad: 25 ciudad: 'Madrid'

nombre: unNombre
    nombre := unNombre

edad: unaEdad
    edad := unaEdad

ciudad: unaCiudad
    ciudad := unaCiudad

imprimir
    Transcript show: 'Nombre: ', nombre, ' Edad: ', edad, ' Ciudad: ', ciudad

**Clase Familia**

Familia clase

| personas |

instancia nueva
    ^ self
    new personas: {}

añadirPersona: unaPersona
    personas atPut: unaPersona nombre, unaPersona

imprimir
    personas do: [:persona | persona imprimir]

**Clase Ciudad**

Ciudad clase

| nombre | habitantes |

instancia nueva
    ^ self
    new nombre: 'Madrid' habitantes: 3000000

nombre: unNombre
    nombre := unNombre

habitantes: unosHabitantes
    habitantes := unosHabitantes

imprimir
    Transcript show: 'Nombre: ', nombre, ' Habitantes: ', habitantes

**Clase Programa**

Programa clase

| familia | ciudad |

instancia nueva
    ^ self
    new familia: Familia nueva, ciudad: Ciudad nueva

ejecutar
    familia imprimir.
    ciudad imprimir.

**Código principal**

Programa nueva ejecutar.

```

Este código crea una clase `Persona` con tres propiedades: `nombre`, `edad` y `ciudad`. La clase `Persona` tiene un método `imprimir` que imprime los valores de sus propiedades.

Luego crea una clase `Familia` que tiene una propiedad `personas` que es un diccionario que mapea los nombres de las personas con las instancias de la clase `Persona`. La clase `Familia` tiene un método `añadirPersona` que añade una persona a la familia y un método `imprimir` que imprime los nombres de todas las personas de la familia.

Después crea una clase `Ciudad` con dos propiedades: `nombre` y `habitantes`. La clase `Ciudad` tiene un método `imprimir` que imprime el nombre y el número de habitantes de la ciudad.

Finalmente crea una clase `Programa` que tiene dos propiedades: `familia` y `ciudad`. La clase `Programa` tiene un método `ejecutar` que imprime la información de la familia y la ciudad.

El código principal crea una instancia de la clase `Programa` y llama al método `ejecutar` para imprimir la información de la familia y la ciudad.