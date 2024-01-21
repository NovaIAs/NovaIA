```javascript
// Función principal
const main = () => {

  // Crear un objeto persona
  const persona = {
    nombre: "Juan",
    apellido: "Pérez",
    edad: 30,
    ocupación: "Desarrollador de software",
    habilidades: ["JavaScript", "Python", "Java"],
    mascotas: [
      {
        nombre: "Firulais",
        especie: "Perro",
        raza: "Golden Retriever"
      },
      {
        nombre: "Michi",
        especie: "Gato",
        raza: "Siamés"
      }
    ],
    amigos: [
      {
        nombre: "María",
        apellido: "López",
        edad: 28,
        ocupación: "Diseñadora gráfica"
      },
      {
        nombre: "Pedro",
        apellido: "García",
        edad: 32,
        ocupación: "Ingeniero civil"
      }
    ]
  };

  // Obtener el nombre de la persona
  const nombre = persona.nombre;

  // Obtener la ocupación de la persona
  const ocupación = persona.ocupación;

  // Obtener la edad de la persona
  const edad = persona.edad;

  // Obtener las habilidades de la persona
  const habilidades = persona.habilidades;

  // Obtener las mascotas de la persona
  const mascotas = persona.mascotas;

  // Obtener los amigos de la persona
  const amigos = persona.amigos;

  // Imprimir en consola la información de la persona
  console.log("Nombre:", nombre);
  console.log("Ocupación:", ocupación);
  console.log("Edad:", edad);
  console.log("Habilidades:", habilidades);
  console.log("Mascotas:", mascotas);
  console.log("Amigos:", amigos);

  // Utilizar la función map() para obtener los nombres de las mascotas
  const nombresMascotas = mascotas.map((mascota) => mascota.nombre);

  // Imprimir en consola los nombres de las mascotas
  console.log("Nombres de las mascotas:", nombresMascotas);

  // Utilizar la función filter() para obtener los amigos que son mayores de 30 años
  const amigosMayores30 = amigos.filter((amigo) => amigo.edad > 30);

  // Imprimir en consola los amigos que son mayores de 30 años
  console.log("Amigos mayores de 30 años:", amigosMayores30);

  // Ordenar los amigos por edad en orden ascendente
  const amigosOrdenadosPorEdad = amigos.sort((a, b) => a.edad - b.edad);

  // Imprimir en consola los amigos ordenados por edad en orden ascendente
  console.log("Amigos ordenados por edad:", amigosOrdenadosPorEdad);

};

// Llamar a la función principal
main();
```

Explicación:

* La función `main()` es la función principal del código. Es la primera función que se ejecuta y llama a otras funciones para realizar tareas específicas.
* La función `main()` crea un objeto `persona` y obtiene información de él, como el nombre, la ocupación, la edad, las habilidades, las mascotas y los amigos.
* La función `main()` imprime en consola la información de la persona.
* La función `main()` utiliza la función `map()` para obtener los nombres de las mascotas.
* La función `main()` utiliza la función `filter()` para obtener los amigos que son mayores de 30 años.
* La función `main()` utiliza la función `sort()` para ordenar los amigos por edad en orden ascendente.
* La función `main()` imprime en consola los nombres de las mascotas, los amigos que son mayores de 30 años y los amigos ordenados por edad en orden ascendente.