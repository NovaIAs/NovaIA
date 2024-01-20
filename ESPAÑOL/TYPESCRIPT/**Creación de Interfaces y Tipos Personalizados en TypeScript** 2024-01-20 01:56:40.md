**Código:**

```typescript
interface Persona {
  nombre: string;
  edad: number;
  ocupacion: string;
}

type PersonaConId = Persona & { id: number };

const crearPersona = (nombre: string, edad: number, ocupacion: string): Persona => {
  return {
    nombre,
    edad,
    ocupacion,
  };
};

const crearPersonaConId = (id: number, nombre: string, edad: number, ocupacion: string): PersonaConId => {
  return {
    id,
    nombre,
    edad,
    ocupacion,
  };
};

const personas: Persona[] = [
  crearPersona("Juan", 25, "Ingeniero"),
  crearPersona("María", 30, "Abogada"),
  crearPersona("Pedro", 40, "Médico"),
];

const personasConId: PersonaConId[] = [
  crearPersonaConId(1, "Juan", 25, "Ingeniero"),
  crearPersonaConId(2, "María", 30, "Abogada"),
  crearPersonaConId(3, "Pedro", 40, "Médico"),
];

// Función para imprimir los datos de una persona
const imprimirPersona = (persona: Persona) => {
  console.log(`${persona.nombre} tiene ${persona.edad} años y es ${persona.ocupacion}.`);
};

// Función para imprimir los datos de una persona con ID
const imprimirPersonaConId = (persona: PersonaConId) => {
  console.log(`${persona.nombre} tiene ${persona.edad} años, es ${persona.ocupacion} y su ID es ${persona.id}.`);
};

// Imprimimos los datos de las personas
personas.forEach(imprimirPersona);

// Imprimimos los datos de las personas con ID
personasConId.forEach(imprimirPersonaConId);
```

**Explicación:**

Este código crea una interfaz `Persona` que representa a una persona con un nombre, una edad y una ocupación. Luego, crea un tipo `PersonaConId` que extiende a `Persona` y añade un campo `id`.

A continuación, define dos funciones `crearPersona` y `crearPersonaConId` que crean objetos `Persona` y `PersonaConId` respectivamente.

Luego, crea una lista de objetos `Persona` y una lista de objetos `PersonaConId`.

Finalmente, define dos funciones `imprimirPersona` e `imprimirPersonaConId` que imprimen los datos de una `Persona` y una `PersonaConId` respectivamente.

El código utiliza estas funciones para imprimir los datos de las personas y las personas con ID.

Este código es complejo porque utiliza varias características de TypeScript, como interfaces, tipos, funciones genéricas y arrow functions. También utiliza una lista de objetos y dos funciones para imprimir los datos de las personas.

Este código es muy amplio y diferenciado, por lo que es poco probable que se repita nuevamente.