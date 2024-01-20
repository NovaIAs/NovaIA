```typescript
// Definición de una interfaz para una persona
interface Persona {
  nombre: string;
  edad: number;
  ocupacion: string;
}

// Creación de una lista de personas
const personas: Persona[] = [
  { nombre: 'Juan', edad: 25, ocupacion: 'Ingeniero' },
  { nombre: 'María', edad: 30, ocupacion: 'Médica' },
  { nombre: 'Pedro', edad: 35, ocupacion: 'Abogado' },
];

// Función para obtener el nombre completo de una persona
function obtenerNombreCompleto(persona: Persona): string {
  return `${persona.nombre} ${persona.edad} ${persona.ocupacion}`;
}

// Función para obtener la edad promedio de las personas
function obtenerEdadPromedio(personas: Persona[]): number {
  let edadTotal = 0;
  for (let i = 0; i < personas.length; i++) {
    edadTotal += personas[i].edad;
  }
  return edadTotal / personas.length;
}

// Función para obtener la ocupación más común entre las personas
function obtenerOcupacionMasComun(personas: Persona[]): string {
  let ocupaciones: { [ocupacion: string]: number } = {};
  for (let i = 0; i < personas.length; i++) {
    let ocupacion = personas[i].ocupacion;
    if (ocupaciones[ocupacion] === undefined) {
      ocupaciones[ocupacion] = 0;
    }
    ocupaciones[ocupacion]++;
  }
  let ocupacionMasComun = '';
  let frecuenciaMasComun = 0;
  for (let ocupacion in ocupaciones) {
    if (ocupaciones[ocupacion] > frecuenciaMasComun) {
      ocupacionMasComun = ocupacion;
      frecuenciaMasComun = ocupaciones[ocupacion];
    }
  }
  return ocupacionMasComun;
}

// Impresión de los resultados
console.log('Lista de personas:');
for (let i = 0; i < personas.length; i++) {
  console.log(`Nombre: ${personas[i].nombre}, Edad: ${personas[i].edad}, Ocupación: ${personas[i].ocupacion}`);
}
console.log(`Nombre completo de la primera persona: ${obtenerNombreCompleto(personas[0])}`);
console.log(`Edad promedio de las personas: ${obtenerEdadPromedio(personas)}`);
console.log(`Ocupación más común entre las personas: ${obtenerOcupacionMasComun(personas)}`);
```

Explicación del código:

1. **Interfaz Persona:** Se define una interfaz `Persona` que representa las propiedades comunes de una persona: `nombre`, `edad` y `ocupacion`.

2. **Creación de una lista de personas:** Se crea una lista de personas utilizando un array de objetos que implementan la interfaz `Persona`.

3. **Función obtenerNombreCompleto:** Se define una función `obtenerNombreCompleto` que recibe una persona como argumento y devuelve un string con el nombre completo de la persona.

4. **Función obtenerEdadPromedio:** Se define una función `obtenerEdadPromedio` que recibe una lista de personas como argumento y devuelve un número con la edad promedio de las personas.

5. **Función obtenerOcupacionMasComun:** Se define una función `obtenerOcupacionMasComun` que recibe una lista de personas como argumento y devuelve un string con la ocupación más común entre las personas.

6. **Impresión de los resultados:** Se imprimen los resultados de las funciones definidas anteriormente, incluyendo la lista de personas, el nombre completo de la primera persona, la edad promedio de las personas y la ocupación más común entre las personas.