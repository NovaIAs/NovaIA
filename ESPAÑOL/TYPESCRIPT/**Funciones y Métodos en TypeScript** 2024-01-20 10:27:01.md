```typescript
// 1. Generar un arreglo de números aleatorios entre 1 y 100

const generarArregloAleatorio = (longitud: number): number[] => {
  const numerosAleatorios: number[] = [];

  for (let i = 0; i < longitud; i++) {
    numerosAleatorios.push(Math.floor(Math.random() * 100) + 1); // Generar un número aleatorio entre 1 y 100
  }

  return numerosAleatorios; // Retornar el arreglo de números aleatorios
};

// 2. Crear una interfaz para representar una persona

interface Persona {
  nombre: string;
  edad: number;
  ocupacion: string;
}

// 3. Crear un arreglo de objetos de tipo Persona

const personas: Persona[] = [
  { nombre: "Ana", edad: 25, ocupacion: "Ingeniera de software" },
  { nombre: "Juan", edad: 30, ocupacion: "Médico" },
  { nombre: "María", edad: 28, ocupacion: "Abogada" },
  { nombre: "Pedro", edad: 35, ocupacion: "Profesor" },
  { nombre: "Rosa", edad: 22, ocupacion: "Estudiante" },
];

// 4. Filtrar las personas menores de 30 años

const personasMenoresDe30 = personas.filter((persona) => persona.edad < 30);

// 5. Ordenar las personas por edad en orden ascendente

const personasOrdenadasPorEdad = personas.sort((a, b) => a.edad - b.edad);

// 6. Obtener los nombres de las personas en un arreglo

const nombresDePersonas = personas.map((persona) => persona.nombre);

// 7. Sumar las edades de todas las personas

const sumaDeEdades = personas.reduce((acumulador, persona) => acumulador + persona.edad, 0);

// 8. Imprimir los resultados en la consola

console.log("Arreglo de números aleatorios:", generarArregloAleatorio(10));
console.log("Arreglo de objetos Persona:", personas);
console.log("Personas menores de 30 años:", personasMenoresDe30);
console.log("Personas ordenadas por edad:", personasOrdenadasPorEdad);
console.log("Nombres de las personas:", nombresDePersonas);
console.log("Suma de las edades:", sumaDeEdades);
```

Explicación del código:

1. La función `generarArregloAleatorio` genera un arreglo de números aleatorios entre 1 y 100.
2. La interfaz `Persona` representa una persona con su nombre, edad y ocupación.
3. El arreglo `personas` contiene objetos de tipo `Persona`.
4. La variable `personasMenoresDe30` contiene las personas menores de 30 años filtradas del arreglo `personas`.
5. La variable `personasOrdenadasPorEdad` contiene las personas ordenadas por edad en orden ascendente.
6. La variable `nombresDePersonas` contiene los nombres de las personas extraídos del arreglo `personas`.
7. La variable `sumaDeEdades` contiene la suma de las edades de todas las personas del arreglo `personas`.
8. Finalmente, se imprimen los resultados en la consola.