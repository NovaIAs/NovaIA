```smalltalk

;; Clase persona
Persona := clase {
    nombre: (String)
    edad: (Integer)
    sexo: (String)
    peso: (Number)
    altura: (Number)
    constructor(nombre, edad, sexo, peso, altura) {
        self setNombre: nombre
        self setEdad: edad
        self setSexo: sexo
        self setPeso: peso
        self setAltura: altura
    }
    nombre { ^nombre }
    edad { ^edad }
    sexo { ^sexo }
    peso { ^peso }
    altura { ^altura }
    to_s {
        "Nombre:\t#{self nombre}\n" +
        "Edad:\t\t#{self edad}\n" +
        "Sexo:\t\t#{self sexo}\n" +
        "Peso:\t\t#{self peso} kg\n" +
        "Altura:\t\t#{self altura} m\n"
    }
}

;; Crear una instancia de la clase Persona
persona1 := Persona new(
    nombre: 'Juan García',
    edad: 30,
    sexo: 'Masculino',
    peso: 80.0,
    altura: 1.80
)

;; Crear una instancia de la clase Persona
persona2 := Persona new(
    nombre: 'María Pérez',
    edad: 25,
    sexo: 'Femenino',
    peso: 60.0,
    altura: 1.70
)

;; Imprimir la información de las personas
println(persona1 to_s)
println(persona2 to_s)

;; Crear una colección de personas
personas := Bag new()
personas add: persona1
personas add: persona2

;; Imprimir la lista de personas
personas do: [|persona| println(persona to_s)]

```

Este código crea una clase Persona con propiedades como el nombre, la edad, el sexo, el peso y la altura. Luego, crea dos instancias de la clase Persona, "persona1" y "persona2", y muestra su información en la consola. Además, crea una colección de personas y agrega las dos instancias a la colección. Finalmente, recorre la colección e imprime la información de cada persona en la consola.