```typescript
// Definir una interfaz para representar una persona.
interface Persona {
  nombre: string;
  edad: number;
  direccion: string;
  telefono: string;
}

// Definir una clase que implementa la interfaz Persona.
class PersonaImpl implements Persona {
  constructor(nombre: string, edad: number, direccion: string, telefono: string) {
    this.nombre = nombre;
    this.edad = edad;
    this.direccion = direccion;
    this.telefono = telefono;
  }

  nombre: string;
  edad: number;
  direccion: string;
  telefono: string;
}

// Definir una función que crea una instancia de la clase PersonaImpl.
function crearPersona(nombre: string, edad: number, direccion: string, telefono: string): Persona {
  return new PersonaImpl(nombre, edad, direccion, telefono);
}

// Definir una función que imprime los datos de una persona.
function imprimirPersona(persona: Persona): void {
  console.log(`Nombre: ${persona.nombre}`);
  console.log(`Edad: ${persona.edad}`);
  console.log(`Dirección: ${persona.direccion}`);
  console.log(`Teléfono: ${persona.telefono}`);
}

// Crear una instancia de la clase PersonaImpl.
const persona1 = crearPersona("Juan", 25, "Calle Mayor, 123", "123456789");

// Imprimir los datos de la persona.
imprimirPersona(persona1);
```

Este código define una interfaz, una clase y una función en TypeScript. La interfaz `Persona` define las propiedades de una persona, como su nombre, edad, dirección y teléfono. La clase `PersonaImpl` implementa la interfaz `Persona` y proporciona una implementación para cada una de sus propiedades. La función `crearPersona` crea una instancia de la clase `PersonaImpl` y la devuelve. La función `imprimirPersona` imprime los datos de una persona.

El código también demuestra cómo utilizar las clases e interfaces en TypeScript. La clase `PersonaImpl` se crea utilizando la palabra clave `class` y la función `crearPersona` se crea utilizando la palabra clave `function`. Las propiedades de la clase `PersonaImpl` se definen utilizando la palabra clave `constructor`. La función `imprimirPersona` utiliza la palabra clave `console.log` para imprimir los datos de una persona.

Este código es un ejemplo de cómo utilizar las clases e interfaces en TypeScript para crear objetos y realizar operaciones sobre ellos.