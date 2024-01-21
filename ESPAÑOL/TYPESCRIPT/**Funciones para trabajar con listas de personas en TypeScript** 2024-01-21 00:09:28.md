```typescript
// Definimos un tipo de dato personalizado llamado `Persona`
type Persona = {
  nombre: string;
  edad: number;
  ocupacion: string;
};

// Creamos una función que recibe una lista de personas y devuelve la edad promedio
function obtenerEdadPromedio(personas: Persona[]): number {
  let sumaEdades = 0;
  for (let i = 0; i < personas.length; i++) {
    sumaEdades += personas[i].edad;
  }
  return sumaEdades / personas.length;
}

// Creamos una función que recibe una lista de personas y devuelve la persona de mayor edad
function obtenerPersonaMayor(personas: Persona[]): Persona {
  let personaMayor = personas[0];
  for (let i = 1; i < personas.length; i++) {
    if (personas[i].edad > personaMayor.edad) {
      personaMayor = personas[i];
    }
  }
  return personaMayor;
}

// Creamos una función que recibe una lista de personas y devuelve las personas que tienen una ocupación determinada
function obtenerPersonasPorOcupacion(personas: Persona[], ocupacion: string): Persona[] {
  let personasPorOcupacion = [];
  for (let i = 0; i < personas.length; i++) {
    if (personas[i].ocupacion === ocupacion) {
      personasPorOcupacion.push(personas[i]);
    }
  }
  return personasPorOcupacion;
}

// Creamos una lista de personas
let personas = [
  { nombre: 'Juan', edad: 25, ocupacion: 'Desarrollador' },
  { nombre: 'María', edad: 30, ocupacion: 'Diseñadora' },
  { nombre: 'Pedro', edad: 35, ocupacion: 'Gerente' },
  { nombre: 'Ana', edad: 40, ocupacion: 'Contadora' },
  { nombre: 'José', edad: 45, ocupacion: 'Abogado' },
];

// Obtenemos la edad promedio de las personas
let edadPromedio = obtenerEdadPromedio(personas);
console.log(`La edad promedio de las personas es: ${edadPromedio}`);

// Obtenemos la persona de mayor edad
let personaMayor = obtenerPersonaMayor(personas);
console.log(`La persona de mayor edad es: ${personaMayor.nombre}`);

// Obtenemos las personas que tienen la ocupación de 'Desarrollador'
let personasDesarrolladoras = obtenerPersonasPorOcupacion(personas, 'Desarrollador');
console.log('Las personas que tienen la ocupación de "Desarrollador" son:');
for (let i = 0; i < personasDesarrolladoras.length; i++) {
  console.log(personasDesarrolladoras[i].nombre);
}
```

Explicación del código:

1. Definimos un tipo de dato personalizado llamado `Persona` que tiene tres propiedades: `nombre`, `edad` y `ocupacion`.
2. Creamos tres funciones: `obtenerEdadPromedio`, `obtenerPersonaMayor` y `obtenerPersonasPorOcupacion`.
3. La función `obtenerEdadPromedio` recibe una lista de personas y devuelve la edad promedio.
4. La función `obtenerPersonaMayor` recibe una lista de personas y devuelve la persona de mayor edad.
5. La función `obtenerPersonasPorOcupacion` recibe una lista de personas y una ocupación específica, y devuelve las personas que tienen esa ocupación.
6. Creamos una lista de personas y la almacenamos en la variable `personas`.
7. Llamamos a la función `obtenerEdadPromedio` con la lista de personas y almacenamos el resultado en la variable `edadPromedio`.
8. Llamamos a la función `obtenerPersonaMayor` con la lista de personas y almacenamos el resultado en la variable `personaMayor`.
9. Llamamos a la función `obtenerPersonasPorOcupacion` con la lista de personas y la ocupación 'Desarrollador', y almacenamos el resultado en la variable `personasDesarrolladoras`.
10. Imprimimos la edad promedio, la persona de mayor edad y las personas que tienen la ocupación de 'Desarrollador' en la consola.