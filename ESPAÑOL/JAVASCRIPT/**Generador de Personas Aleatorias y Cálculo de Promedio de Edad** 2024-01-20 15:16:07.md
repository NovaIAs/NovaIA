```javascript
// Crear una clase Persona con la estructura nombre, apellido, edad y género.
class Persona {
  constructor(nombre, apellido, edad, género) {
    this.nombre = nombre;
    this.apellido = apellido;
    this.edad = edad;
    this.género = género;
  }

  // Definir el método saludar() para cada persona.
  saludar() {
    console.log(`Hola, soy ${this.nombre} ${this.apellido} y tengo ${this.edad} años.`);
  }
}

// Crear una función que genere personas aleatorias.
function generarPersonaAleatoria() {
  // Generar nombres y apellidos aleatorios.
  const nombres = ["Juan", "María", "Pedro", "Ana", "José", "Rosa", "Carlos", "Marta", "Luis", "Lucía"];
  const apellidos = ["García", "López", "Martínez", "Sánchez", "Ruiz", "Díaz", "Hernández", "Moreno", "Fernández", "Jiménez"];

  // Generar edad y género aleatorios.
  const edad = Math.floor(Math.random() * 100);
  const género = Math.random() > 0.5 ? "Masculino" : "Femenino";

  // Crear y devolver una nueva persona.
  return new Persona(
    nombres[Math.floor(Math.random() * nombres.length)],
    apellidos[Math.floor(Math.random() * apellidos.length)],
    edad,
    género
  );
}

// Crear una función que genere una lista de personas aleatorias.
function generarListaPersonasAleatorias(cantidad) {
  const listaPersonas = [];

  // Generar la cantidad especificada de personas aleatorias y agregarlas a la lista.
  for (let i = 0; i < cantidad; i++) {
    listaPersonas.push(generarPersonaAleatoria());
  }

  // Devolver la lista de personas.
  return listaPersonas;
}

// Crear una función que calcule el promedio de edad de una lista de personas.
function calcularPromedioEdad(listaPersonas) {
  // Sumar las edades de todas las personas en la lista.
  let sumaEdades = 0;
  for (const persona of listaPersonas) {
    sumaEdades += persona.edad;
  }

  // Dividir la suma de edades entre la cantidad de personas para obtener el promedio.
  const promedioEdad = sumaEdades / listaPersonas.length;

  // Devolver el promedio de edad.
  return promedioEdad;
}

// Crear una lista de 10 personas aleatorias.
const listaPersonas = generarListaPersonasAleatorias(10);

// Mostrar la lista de personas.
console.log("Lista de personas:");
for (const persona of listaPersonas) {
  persona.saludar();
}

// Calcular el promedio de edad de la lista de personas.
const promedioEdad = calcularPromedioEdad(listaPersonas);

// Mostrar el promedio de edad.
console.log(`El promedio de edad de la lista de personas es: ${promedioEdad}`);
```

Explicación del código:

* La clase `Persona` define la estructura de datos para las personas, con los atributos `nombre`, `apellido`, `edad` y `género`. El método `saludar()` imprime un saludo para cada persona.
* La función `generarPersonaAleatoria()` genera una nueva persona aleatoria con atributos aleatorios.
* La función `generarListaPersonasAleatorias()` genera una lista de personas aleatorias de un tamaño especificado.
* La función `calcularPromedioEdad()` calcula el promedio de edad de una lista de personas.
* El código principal crea una lista de 10 personas aleatorias, muestra la lista y calcula el promedio de edad de la lista.