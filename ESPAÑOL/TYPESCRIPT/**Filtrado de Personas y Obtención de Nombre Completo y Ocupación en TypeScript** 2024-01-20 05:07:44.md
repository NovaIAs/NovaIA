```typescript
// Definición de una interfaz para representar una persona.
interface Persona {
  nombre: string;
  apellido: string;
  edad: number;
  ocupación: string;
}

// Creación de un array de objetos Persona.
const personas = [
  {
    nombre: "Juan",
    apellido: "Pérez",
    edad: 25,
    ocupación: "Ingeniero"
  },
  {
    nombre: "María",
    apellido: "García",
    edad: 30,
    ocupación: "Doctora"
  },
  {
    nombre: "Pedro",
    apellido: "López",
    edad: 35,
    ocupación: "Abogado"
  }
];

// Función para filtrar el array de personas por edad.
const filtrarPorEdad = (personas: Persona[], edad: number) => {
  return personas.filter(persona => persona.edad > edad);
};

// Función para obtener el nombre completo de una persona.
const obtenerNombreCompleto = (persona: Persona) => {
  return `${persona.nombre} ${persona.apellido}`;
};

// Función para imprimir el nombre completo y la ocupación de una persona.
const imprimirPersona = (persona: Persona) => {
  console.log(`${obtenerNombreCompleto(persona)} - ${persona.ocupación}`);
};

// Utilización de las funciones para filtrar el array de personas, obtener el nombre completo y la ocupación, e imprimir la información.
const personasFiltradas = filtrarPorEdad(personas, 30);
personasFiltradas.forEach(persona => {
  imprimirPersona(persona);
});

```

Este código crea un arreglo de objetos `Persona`, y los filtra para obtener a las personas mayores de 30 años.
Luego, se utiliza una función para obtener el nombre completo y la ocupación de cada persona, y otra función para imprimir esta información en la consola.

Espero que esto te sea útil!