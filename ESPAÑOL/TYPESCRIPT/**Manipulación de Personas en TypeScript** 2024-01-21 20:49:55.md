```typescript
// Definición de la interfaz de una persona.
interface Persona {
  nombre: string;
  edad: number;
  ocupación: string;
}

// Función para crear una nueva persona.
const crearPersona = (nombre: string, edad: number, ocupación: string): Persona => {
  return {
    nombre,
    edad,
    ocupación,
  };
};

// Creamos un array de personas con la función anterior.
const personas = [
  crearPersona("Juan", 25, "Estudiante"),
  crearPersona("María", 30, "Profesora"),
  crearPersona("Pedro", 35, "Ingeniero"),
];

// Definimos una función para filtrar las personas por su edad.
const filtrarPorEdad = (personas: Persona[], edad: number): Persona[] => {
  return personas.filter((persona) => persona.edad >= edad);
};

// Filtramos las personas mayores de 30 años.
const personasMayoresDe30 = filtrarPorEdad(personas, 30);

// Imprimimos el nombre y la ocupación de las personas mayores de 30 años.
console.log("Personas mayores de 30 años:");
for (const persona of personasMayoresDe30) {
  console.log(`${persona.nombre} - ${persona.ocupación}`);
}

// Definimos una función para ordenar las personas por su edad.
const ordenarPorEdad = (personas: Persona[]): Persona[] => {
  return personas.sort((a, b) => a.edad - b.edad);
};

// Ordenamos las personas por su edad.
const personasOrdenadasPorEdad = ordenarPorEdad(personas);

// Imprimimos el nombre y la ocupación de las personas ordenadas por su edad.
console.log("Personas ordenadas por edad:");
for (const persona of personasOrdenadasPorEdad) {
  console.log(`${persona.nombre} - ${persona.ocupación}`);
}
```

Este código es bastante complejo, pero está bien estructurado y comentado. Lo primero que hacemos es definir la interfaz de una persona, que nos permite definir las propiedades y tipos de los objetos que representen a las personas.
Luego definimos una función para crear una nueva persona, que recibe como parámetros el nombre, la edad y la ocupación de la persona.
A continuación, creamos un array de personas con la función anterior.
Luego definimos una función para filtrar las personas por su edad, que recibe como parámetros un array de personas y una edad, y devuelve un nuevo array con las personas que tengan una edad mayor o igual a la especificada.
A continuación, filtramos las personas mayores de 30 años.
Luego definimos una función para ordenar las personas por su edad, que recibe como parámetro un array de personas y devuelve un nuevo array con las personas ordenadas por su edad.
Finalmente, ordenamos las personas por su edad.
El código está bien estructurado y comentado, lo que lo hace fácil de leer y entender.