```go
// Definición de una estructura "Persona".
type Persona struct {
    nombre string
    edad   int
    ciudad string
}

// Definición de una interfaz "Hablador".
type Hablador interface {
    Hablar() string
}

// Implementación de la interfaz "Hablador" para la estructura "Persona".
func (p Persona) Hablar() string {
    return "Hola, mi nombre es " + p.nombre
}

// Definición de una función "Saludar" que recibe un parámetro de tipo "Hablador".
func Saludar(h Hablador) {
    fmt.Println(h.Hablar())
}

// Función principal.
func main() {
    // Creación de un objeto de tipo "Persona".
    persona1 := Persona{nombre: "Juan", edad: 25, ciudad: "Madrid"}

    // Usando la interfaz "Hablador" para llamar a la función "Hablar" del objeto "persona1".
    Saludar(persona1)

    // Definición de una estructura "Animal".
    type Animal struct {
        especie string
        sonido  string
    }

    // Definición de una interfaz "EmitirSonido".
    type EmitirSonido interface {
        EmitirSonido() string
    }

    // Implementación de la interfaz "EmitirSonido" para la estructura "Animal".
    func (a Animal) EmitirSonido() string {
        return "El " + a.especie + " hace " + a.sonido
    }

    // Función "HacerRuido" que recibe un parámetro de tipo "EmitirSonido".
    func HacerRuido(e EmitirSonido) {
        fmt.Println(e.EmitirSonido())
    }

    // Creación de un objeto de tipo "Animal".
    animal1 := Animal{especie: "Perro", sonido: "Guau"}

    // Usando la interfaz "EmitirSonido" para llamar a la función "EmitirSonido" del objeto "animal1".
    HacerRuido(animal1)
}
```

Explicación del código:

* Definimos dos estructuras: "Persona" y "Animal". Cada estructura tiene sus propios campos.
* Definimos dos interfaces: "Hablador" y "EmitirSonido". Cada interfaz tiene un método.
* Implementamos las interfaces "Hablador" y "EmitirSonido" para las estructuras "Persona" y "Animal" respectivamente.
* Definimos dos funciones: "Saludar" y "HacerRuido". Cada función recibe un parámetro de tipo "Hablador" y "EmitirSonido" respectivamente.
* En la función principal, creamos objetos de tipo "Persona" y "Animal" y los usamos para llamar a las funciones "Saludar" y "HacerRuido".

Este código muestra cómo usar interfaces para lograr la abstracción en Go. Las interfaces permiten definir un conjunto de métodos que deben ser implementados por cualquier tipo que quiera usar la interfaz. Esto hace que sea más fácil escribir código modular y reutilizable.