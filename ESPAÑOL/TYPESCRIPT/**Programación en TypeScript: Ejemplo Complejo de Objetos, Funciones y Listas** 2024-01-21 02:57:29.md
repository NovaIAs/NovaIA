```typescript
// Ejemplo de código complejo en TypeScript

// Definir una interfaz para representar objetos de tipo Persona
interface Persona {
  nombre: string;
  edad: number;
}

// Definir una clase que implemente la interfaz Persona
class PersonaImpl implements Persona {
  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  nombre: string;
  edad: number;
}

// Definir una función que reciba una lista de objetos de tipo Persona y devuelva una lista de nombres de las personas mayores de edad
function obtenerNombresMayoresDeEdad(personas: Persona[]): string[] {
  return personas.filter((persona) => persona.edad >= 18).map((persona) => persona.nombre);
}

// Definir una función que reciba una lista de objetos de tipo Persona y devuelva un objeto que contenga el nombre y la edad promedio de las personas
function obtenerPromedioEdad(personas: Persona[]): { nombrePromedio: string; edadPromedio: number } {
  const nombrePromedio = personas.map((persona) => persona.nombre).join(" ");
  const edadPromedio = personas.reduce((acc, persona) => acc + persona.edad, 0) / personas.length;
  return { nombrePromedio, edadPromedio };
}

// Crear una lista de objetos de tipo Persona
const personas: Persona[] = [
  new PersonaImpl("Juan", 25),
  new PersonaImpl("María", 30),
  new PersonaImpl("Pedro", 15),
  new PersonaImpl("Ana", 20),
];

// Imprimir los nombres de las personas mayores de edad
console.log("Nombres de las personas mayores de edad:");
console.log(obtenerNombresMayoresDeEdad(personas));

// Imprimir el nombre y la edad promedio de las personas
console.log("Nombre y edad promedio de las personas:");
console.log(obtenerPromedioEdad(personas));
```

Explicación del código:

1. Se define una interfaz llamada `Persona` que representa un objeto con dos propiedades: `nombre` (tipo string) y `edad` (tipo number).

2. Se define una clase llamada `PersonaImpl` que implementa la interfaz `Persona`. Esta clase tiene un constructor que inicializa las propiedades `nombre` y `edad` con los valores pasados como argumentos.

3. Se definen dos funciones:

   - `obtenerNombresMayoresDeEdad`: Esta función recibe una lista de objetos de tipo `Persona` y devuelve una lista de nombres de las personas mayores de edad.
   - `obtenerPromedioEdad`: Esta función recibe una lista de objetos de tipo `Persona` y devuelve un objeto que contiene el nombre promedio y la edad promedio de las personas.

4. Se crea una lista de objetos de tipo `Persona` llamada `personas`.

5. Se imprime en la consola los nombres de las personas mayores de edad utilizando la función `obtenerNombresMayoresDeEdad`.

6. Se imprime en la consola el nombre y la edad promedio de las personas utilizando la función `obtenerPromedioEdad`.