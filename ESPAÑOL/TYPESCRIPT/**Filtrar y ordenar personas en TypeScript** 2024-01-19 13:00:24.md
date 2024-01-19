```typescript
// Definir una interfaz para representar una persona
interface Persona {
    nombre: string;
    apellido: string;
    edad: number;
    ocupacion: string;
}

// Crear una clase que implemente la interfaz Persona
class PersonaImplementacion implements Persona {
    constructor(
        public nombre: string,
        public apellido: string,
        public edad: number,
        public ocupacion: string
    ) {}
}

// Definir una función para crear una persona
function crearPersona(nombre: string, apellido: string, edad: number, ocupacion: string): Persona {
    return new PersonaImplementacion(nombre, apellido, edad, ocupacion);
}

// Crear un array de personas
const personas: Persona[] = [
    crearPersona("Juan", "Perez", 25, "Ingeniero"),
    crearPersona("Maria", "Gomez", 30, "Doctora"),
    crearPersona("Pedro", "Rodriguez", 35, "Abogado"),
    crearPersona("Ana", "Fernandez", 40, "Maestra")
];

// Definir una función para filtrar personas por edad
function filtrarPersonasPorEdad(personas: Persona[], edadMinima: number, edadMaxima: number): Persona[] {
    return personas.filter((persona) => persona.edad >= edadMinima && persona.edad <= edadMaxima);
}

// Definir una función para ordenar personas por apellido
function ordenarPersonasPorApellido(personas: Persona[]): Persona[] {
    return personas.sort((a, b) => {
        if (a.apellido < b.apellido) {
            return -1;
        } else if (a.apellido > b.apellido) {
            return 1;
        } else {
            return 0;
        }
    });
}

// Definir una función para imprimir personas
function imprimirPersonas(personas: Persona[]): void {
    for (const persona of personas) {
        console.log(`${persona.nombre} ${persona.apellido} (${persona.edad} años, ${persona.ocupacion})`);
    }
}

// Obtener todas las personas con edades entre 20 y 40 años
const personasFiltradasPorEdad = filtrarPersonasPorEdad(personas, 20, 40);

// Ordenar las personas filtradas por apellido
const personasOrdenadasPorApellido = ordenarPersonasPorApellido(personasFiltradasPorEdad);

// Imprimir las personas ordenadas por apellido
imprimirPersonas(personasOrdenadasPorApellido);

// Resultado esperado:
//
// Juan Perez (25 años, Ingeniero)
// Ana Fernandez (40 años, Maestra)
// Maria Gomez (30 años, Doctora)
```

Explicación del código:

1. Definimos una interfaz `Persona` para representar una persona con sus propiedades `nombre`, `apellido`, `edad` y `ocupacion`.
2. Creamos una clase `PersonaImplementacion` que implementa la interfaz `Persona`. Esta clase es la plantilla para crear objetos persona.
3. Definimos una función `crearPersona` que toma como argumentos los valores de las propiedades de una persona y retorna un objeto `Persona`.
4. Creamos un array de personas utilizando la función `crearPersona`.
5. Definimos una función `filtrarPersonasPorEdad` que toma como argumentos un array de personas y dos números que representan una edad mínima y máxima. Esta función filtra las personas del array que tienen edades entre la edad mínima y máxima.
6. Definimos una función `ordenarPersonasPorApellido` que toma como argumento un array de personas y lo ordena por apellido.
7. Definimos una función `imprimirPersonas` que toma como argumento un array de personas y las imprime en la consola.
8. Obtenemos todas las personas con edades entre 20 y 40 años utilizando la función `filtrarPersonasPorEdad`.
9. Ordenamos las personas filtradas por apellido utilizando la función `ordenarPersonasPorApellido`.
10. Imprimimos las personas ordenadas por apellido utilizando la función `imprimirPersonas`.

Este código muestra cómo crear y manipular objetos en TypeScript, usar funciones, arrays, clases e interfaces, y cómo obtener resultados a través de la consola.