```typescript
// Modelo de datos
interface Persona {
  nombre: string;
  edad: number;
  ocupacion: string;
}

// Lista de personas
const personas: Persona[] = [
  { nombre: "Juan", edad: 25, ocupacion: "Ingeniero" },
  { nombre: "María", edad: 30, ocupacion: "Doctora" },
  { nombre: "Pedro", edad: 35, ocupacion: "Abogado" },
];

// Función para obtener la lista de ocupaciones
const getOcupaciones = (personas: Persona[]): string[] => {
  const ocupaciones: Set<string> = new Set();
  personas.forEach((persona) => {
    ocupaciones.add(persona.ocupacion);
  });
  return Array.from(ocupaciones);
};

// Función para obtener la edad promedio de las personas
const getEdadPromedio = (personas: Persona[]): number => {
  const edades: number[] = personas.map((persona) => persona.edad);
  return edades.reduce((a, b) => a + b, 0) / edades.length;
};

// Función para obtener la persona más joven
const getPersonaMasJoven = (personas: Persona[]): Persona => {
  let personaMasJoven = personas[0];
  for (let i = 1; i < personas.length; i++) {
    if (personas[i].edad < personaMasJoven.edad) {
      personaMasJoven = personas[i];
    }
  }
  return personaMasJoven;
};

// Función para obtener la persona más vieja
const getPersonaMasVieja = (personas: Persona[]): Persona => {
  let personaMasVieja = personas[0];
  for (let i = 1; i < personas.length; i++) {
    if (personas[i].edad > personaMasVieja.edad) {
      personaMasVieja = personas[i];
    }
  }
  return personaMasVieja;
};

// Impresión de los resultados
console.log("Lista de ocupaciones:");
console.log(getOcupaciones(personas));

console.log("Edad promedio:");
console.log(getEdadPromedio(personas));

console.log("Persona más joven:");
console.log(getPersonaMasJoven(personas));

console.log("Persona más vieja:");
console.log(getPersonaMasVieja(personas));
```

Explicación del código:

1. **Modelo de datos**: Definimos una interfaz `Persona` para representar la información de cada persona. Esta interfaz contiene tres propiedades: nombre, edad y ocupación.

2. **Lista de personas**: Creamos una lista de objetos `Pessoa` llamada `pessoas`, que contiene información ficticia sobre tres personas.

3. **Función para obtener la lista de ocupaciones**: Definimos una función `getOcupaciones` que recibe una lista de personas y devuelve una lista de sus ocupaciones. Utilizamos un conjunto (Set) para almacenar las ocupaciones para asegurarnos de que no haya duplicados.

4. **Función para obtener la edad promedio de las personas**: Definimos una función `getEdadPromedio` que recibe una lista de personas y devuelve la edad promedio de ellas. Utilizamos el método `reduce` para sumar las edades de todas las personas y luego dividir ese total por el número de personas.

5. **Función para obtener la persona más joven**: Definimos una función `getPersonaMasJoven` que recibe una lista de personas y devuelve la persona más joven. Utilizamos un ciclo `for` para comparar las edades de todas las personas y encontrar la persona con la edad más baja.

6. **Función para obtener la persona más vieja**: Definimos una función `getPersonaMasVieja` que recibe una lista de personas y devuelve la persona más vieja. Utilizamos un ciclo `for` para comparar las edades de todas las personas y encontrar la persona con la edad más alta.

7. **Impresión de los resultados**: Finalmente, utilizamos `console.log` para imprimir los resultados en la consola. Imprimimos la lista de ocupaciones, la edad promedio, la persona más joven y la persona más vieja.