```javascript
// Creación de un objeto `Persona` con propiedades y métodos
class Persona {
  constructor(nombre, edad, ocupacion) {
    this.nombre = nombre;
    this.edad = edad;
    this.ocupacion = ocupacion;
  }

  saludar() {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años. Soy ${this.ocupacion}.`);
  }
}

// Creación de un array de objetos `Persona`
const personas = [
  new Persona("Juan", 25, "Ingeniero"),
  new Persona("María", 30, "Médica"),
  new Persona("Pedro", 40, "Profesor"),
  new Persona("Ana", 20, "Estudiante"),
];

// Función para filtrar el array `personas` por edad
const filtrarPorEdad = (edadMinima) => {
  return personas.filter((persona) => persona.edad >= edadMinima);
};

// Función para ordenar el array `personas` por nombre
const ordenarPorNombre = () => {
  return personas.sort((a, b) => a.nombre.localeCompare(b.nombre));
};

// Impresión de la lista de personas filtradas por edad
console.log("Lista de personas filtradas por edad (mayores de 25 años):");
const personasFiltradas = filtrarPorEdad(25);
personasFiltradas.forEach((persona) => persona.saludar());

// Impresión de la lista de personas ordenadas por nombre
console.log("Lista de personas ordenadas por nombre:");
const personasOrdenadas = ordenarPorNombre();
personasOrdenadas.forEach((persona) => persona.saludar());
```

Explicación del código:

1. Creamos una clase `Persona` que representa una persona con propiedades como nombre, edad y ocupación. También definimos un método `saludar()` que imprime un saludo.
2. Creamos un array `personas` de objetos `Persona` con algunos ejemplos.
3. Definimos una función `filtrarPorEdad` que filtra el array `personas` por edad, devolviendo solo las personas que tengan una edad igual o superior a la especificada.
4. Definimos una función `ordenarPorNombre` que ordena el array `personas` por nombre en orden alfabético.
5. Imprimimos la lista de personas filtradas por edad y la lista de personas ordenadas por nombre, llamando al método `saludar()` de cada persona para mostrar su información.