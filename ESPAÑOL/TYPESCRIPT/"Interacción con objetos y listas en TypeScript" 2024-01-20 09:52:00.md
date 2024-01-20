```typescript
// Define una interfaz para representar una persona.
interface Persona {
  nombre: string;
  edad: number;
  ocupacion: string;
}

// Define una función para crear una nueva persona.
function crearPersona(nombre: string, edad: number, ocupacion: string): Persona {
  return {
    nombre,
    edad,
    ocupacion,
  };
}

// Define una lista de personas.
const personas: Persona[] = [];

// Añade algunas personas a la lista.
personas.push(crearPersona("Juan", 25, "Ingeniero"));
personas.push(crearPersona("María", 30, "Médica"));
personas.push(crearPersona("Pedro", 40, "Abogado"));

// Define una función para imprimir los datos de una persona.
function imprimirPersona(persona: Persona) {
  console.log(`Nombre: ${persona.nombre}`);
  console.log(`Edad: ${persona.edad}`);
  console.log(`Ocupación: ${persona.ocupacion}`);
  console.log("---------------------------");
}

// Imprime los datos de todas las personas.
for (const persona of personas) {
  imprimirPersona(persona);
}

// Define una función para filtrar las personas por ocupación.
function filtrarPersonasPorOcupacion(ocupacion: string): Persona[] {
  return personas.filter((persona) => persona.ocupacion === ocupacion);
}

// Imprime los datos de las personas que son ingenieros.
const ingenieros = filtrarPersonasPorOcupacion("Ingeniero");
console.log("Ingenieros:");
for (const ingeniero of ingenieros) {
  imprimirPersona(ingeniero);
}

// Define una función para ordenar las personas por edad.
function ordenarPersonasPorEdad(): Persona[] {
  return personas.sort((a, b) => a.edad - b.edad);
}

// Imprime los datos de las personas ordenadas por edad.
const personasOrdenadasPorEdad = ordenarPersonasPorEdad();
console.log("Personas ordenadas por edad:");
for (const persona of personasOrdenadasPorEdad) {
  imprimirPersona(persona);
}
```

Explicación:

* La interfaz `Persona` define las propiedades comunes a todas las personas.
* La función `crearPersona` crea una nueva persona a partir de un nombre, una edad y una ocupación.
* La lista `personas` almacena las personas creadas.
* La función `imprimirPersona` imprime los datos de una persona en la consola.
* La función `filtrarPersonasPorOcupacion` filtra las personas por ocupación y devuelve una lista con las personas que coinciden con la ocupación especificada.
* La función `ordenarPersonasPorEdad` ordena las personas por edad y devuelve una lista con las personas ordenadas de menor a mayor edad.