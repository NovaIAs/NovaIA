```typescript
// Definir una interfaz para representar una persona
interface Persona {
    nombre: string;
    edad: number;
    ocupacion: string;
}

// Crear una función para generar una persona aleatoria
function generarPersonaAleatoria(): Persona {
    const nombres = ['Juan', 'María', 'Pedro', 'Ana', 'José'];
    const ocupaciones = ['Estudiante', 'Profesor', 'Ingeniero', 'Médico', 'Abogado'];
    const edadMin = 18;
    const edadMax = 65;

    // Generar un nombre aleatorio
    const nombre = nombres[Math.floor(Math.random() * nombres.length)];

    // Generar una edad aleatoria
    const edad = Math.floor(Math.random() * (edadMax - edadMin + 1)) + edadMin;

    // Generar una ocupación aleatoria
    const ocupacion = ocupaciones[Math.floor(Math.random() * ocupaciones.length)];

    // Devolver la persona generada
    return {
        nombre,
        edad,
        ocupacion
    };
}

// Crear una lista de 10 personas aleatorias
const personas = [];
for (let i = 0; i < 10; i++) {
    personas.push(generarPersonaAleatoria());
}

// Mostrar la lista de personas en la consola
console.log(personas);

// Crear una función para filtrar a las personas mayores de 30 años
function filtrarPersonasMayoresDe30(personas: Persona[]): Persona[] {
    return personas.filter(persona => persona.edad > 30);
}

// Mostrar la lista de personas mayores de 30 años en la consola
console.log(filtrarPersonasMayoresDe30(personas));

// Crear una función para agrupar a las personas por su ocupación
function agruparPersonasPorOcupacion(personas: Persona[]): { [ocupacion: string]: Persona[] } {
    const grupos = {};

    for (const persona of personas) {
        const ocupacion = persona.ocupacion;

        if (!grupos[ocupacion]) {
            grupos[ocupacion] = [];
        }

        grupos[ocupacion].push(persona);
    }

    return grupos;
}

// Mostrar la lista de personas agrupadas por su ocupación en la consola
console.log(agruparPersonasPorOcupacion(personas));
```

Este código crea una lista de 10 personas aleatorias, filtra a las personas mayores de 30 años y agrupa a las personas por su ocupación.

El código está dividido en varias funciones:

* `generarPersonaAleatoria()`: Esta función genera una persona aleatoria.
* `filtrarPersonasMayoresDe30()`: Esta función filtra a las personas mayores de 30 años.
* `agruparPersonasPorOcupacion()`: Esta función agrupa a las personas por su ocupación.

El código utiliza la interfaz `Persona` para representar a las personas. La interfaz define tres propiedades: `nombre`, `edad` y `ocupacion`.

El código también utiliza la biblioteca `lodash` para facilitar la manipulación de listas.

El código se muestra en la consola de forma legible y fácil de entender.