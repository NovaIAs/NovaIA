```typescript
// Crear una interfaz para definir el esquema de un objeto.
interface Persona {
  nombre: string;
  edad: number;
  pasatiempos: string[];
  direccion?: {
    calle: string;
    numero: number;
    ciudad: string;
    codigoPostal: string;
  };
}

// Crear una clase que implemente la interfaz Persona.
class PersonaImpl implements Persona {
  constructor(
    public nombre: string,
    public edad: number,
    public pasatiempos: string[],
    public direccion?: {
      calle: string;
      numero: number;
      ciudad: string;
      codigoPostal: string;
    }
  ) {}
}

// Crear una función que reciba una lista de objetos Persona y devuelva una lista de sus nombres.
const obtenerNombres = (personas: Persona[]): string[] => {
  return personas.map(persona => persona.nombre);
};

// Crear una función que reciba dos objetos Persona y devuelva cierto si son iguales y falso en caso contrario.
const sonIguales = (persona1: Persona, persona2: Persona): boolean => {
  return (
    persona1.nombre === persona2.nombre &&
    persona1.edad === persona2.edad &&
    persona1.pasatiempos.length === persona2.pasatiempos.length &&
    persona1.pasatiempos.every((pasatiempo, index) => pasatiempo === persona2.pasatiempos[index]) &&
    persona1.direccion?.calle === persona2.direccion?.calle &&
    persona1.direccion?.numero === persona2.direccion?.numero &&
    persona1.direccion?.ciudad === persona2.direccion?.ciudad &&
    persona1.direccion?.codigoPostal === persona2.direccion?.codigoPostal
  );
};

// Crear una función que reciba un objeto Persona y devuelva una cadena con su información.
const obtenerInformacion = (persona: Persona): string => {
  return `Nombre: ${persona.nombre}\nEdad: ${persona.edad}\nPasatiempos: ${persona.pasatiempos.join(', ')}\nDirección: ${
    persona.direccion
      ? `${persona.direccion.calle} ${persona.direccion.numero}, ${persona.direccion.ciudad}, ${persona.direccion.codigoPostal}`
      : 'No disponible'
  }`;
};

// Crear una lista de objetos Persona.
const personas: Persona[] = [
  new PersonaImpl('Juan', 25, ['Leer', 'Viajar', 'Jugar videojuegos']),
  new PersonaImpl('María', 30, ['Cocinar', 'Escuchar música', 'Ver películas']),
  new PersonaImpl('Pedro', 35, ['Hacer deporte', 'Ir al gimnasio', 'Salir con amigos']),
  new PersonaImpl('Ana', 40, ['Leer', 'Viajar', 'Ir de compras']),
];

// Obtener la lista de nombres de las personas.
const nombres = obtenerNombres(personas);
console.log('Nombres de las personas:', nombres);

// Obtener la información de la primera persona.
const informacionPrimeraPersona = obtenerInformacion(personas[0]);
console.log('Información de la primera persona:', informacionPrimeraPersona);

// Comprobar si la primera y la segunda persona son iguales.
const sonIgualesPrimeraYSegundaPersona = sonIguales(personas[0], personas[1]);
console.log('¿Son iguales la primera y la segunda persona?', sonIgualesPrimeraYSegundaPersona);

// Comprobar si la primera y la tercera persona son iguales.
const sonIgualesPrimeraYTerceraPersona = sonIguales(personas[0], personas[2]);
console.log('¿Son iguales la primera y la tercera persona?', sonIgualesPrimeraYTerceraPersona);
```

Explicación del código:

* Se crea una interfaz `Persona` que define el esquema de un objeto que representa a una persona. La interfaz incluye propiedades para el nombre, la edad, los pasatiempos y la dirección de la persona.
* Se crea una clase `PersonaImpl` que implementa la interfaz `Persona`. Esta clase representa una implementación concreta de una persona.
* Se crean varias funciones que reciben objetos `Persona` y realizan diversas operaciones sobre ellos, como obtener la lista de nombres de las personas, obtener la información de una persona, y comprobar si dos personas son iguales.
* Se crea una lista de objetos `Persona` y se almacenan en la variable `personas`.
* Se llama a las funciones definidas anteriormente para realizar diversas operaciones sobre la lista de personas y se muestran los resultados en la consola.