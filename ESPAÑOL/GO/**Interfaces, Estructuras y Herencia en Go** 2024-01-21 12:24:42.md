```go
// Definimos una interfaz "IPersona" con métodos básicos de una persona.
type IPersona interface {
	Nombre() string
	Edad() int
	Hablar()
}

// Creamos una struct "Persona" que implementa la interfaz "IPersona".
type Persona struct {
	nombre  string
	edad    int
	idioma  string
}

// Implementamos los métodos de la interfaz "IPersona" para la struct "Persona".
func (p Persona) Nombre() string {
	return p.nombre
}

func (p Persona) Edad() int {
	return p.edad
}

func (p Persona) Hablar() {
	fmt.Println("Hola, mi nombre es", p.nombre, "y tengo", p.edad, "años.")
	fmt.Println("Puedo hablar", p.idioma)
}

// Creamos una struct "Estudiante" que hereda de la struct "Persona".
type Estudiante struct {
	Persona
	matricula string
}

// Redefinimos el método "Hablar()" para la struct "Estudiante".
func (e Estudiante) Hablar() {
	// Llamamos al método "Hablar()" de la struct "Persona" con la palabra clave "super".
	super.Hablar()
	fmt.Println("Soy un estudiante y mi matrícula es", e.matricula)
}

// Creamos una struct "Profesor" que hereda de la struct "Persona".
type Profesor struct {
	Persona
	asignatura string
}

// Redefinimos el método "Hablar()" para la struct "Profesor".
func (p Profesor) Hablar() {
	// Llamamos al método "Hablar()" de la struct "Persona" con la palabra clave "super".
	super.Hablar()
	fmt.Println("Soy un profesor y enseño", p.asignatura)
}

// Creamos una función principal.
func main() {
	// Creamos una instancia de la struct "Persona".
	persona := Persona{
		nombre:  "Juan",
		edad:    25,
		idioma:  "español",
	}

	// Creamos una instancia de la struct "Estudiante".
	estudiante := Estudiante{
		Persona: Persona{
			nombre:  "María",
			edad:    20,
			idioma:  "inglés",
		},
		matricula: "12345",
	}

	// Creamos una instancia de la struct "Profesor".
	profesor := Profesor{
		Persona: Persona{
			nombre:  "Pedro",
			edad:    30,
			idioma:  "francés",
		},
		asignatura: "matemáticas",
	}

	// Llamamos al método "Hablar()" de cada instancia.
	persona.Hablar()
	estudiante.Hablar()
	profesor.Hablar()
}
```

Explicación:

* En este código definimos una interfaz `IPersona` con los métodos básicos de una persona: `Nombre()`, `Edad()` y `Hablar()`.
* Creamos la struct `Persona` que implementa la interfaz `IPersona`.
* Creamos la struct `Estudiante` que hereda de la struct `Persona` y añade un atributo `matricula`.
* Creamos la struct `Profesor` que hereda de la struct `Persona` y añade un atributo `asignatura`.
* Redefinimos el método `Hablar()` para las structs `Estudiante` y `Profesor` para que llamen al método `Hablar()` de la struct `Persona` y añadan información adicional.
* En la función principal creamos instancias de las structs `Persona`, `Estudiante` y `Profesor` y llamamos al método `Hablar()` de cada instancia.

Este código muestra cómo crear interfaces, structs y herencia en Go.