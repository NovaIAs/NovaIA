```javascript
// Función para crear un objeto con los datos de una persona
const crearPersona = (nombre, apellido, edad, ciudad) => {
  return {
    nombre,
    apellido,
    edad,
    ciudad,
  };
};

// Función para obtener el nombre completo de una persona
const obtenerNombreCompleto = (persona) => {
  return `${persona.nombre} ${persona.apellido}`;
};

// Función para obtener la edad de una persona
const obtenerEdad = (persona) => {
  return persona.edad;
};

// Función para obtener la ciudad de una persona
const obtenerCiudad = (persona) => {
  return persona.ciudad;
};

// Función para crear un array con los datos de varias personas
const crearArrayPersonas = (personas) => {
  return personas.map((persona) => {
    return crearPersona(persona.nombre, persona.apellido, persona.edad, persona.ciudad);
  });
};

// Función para obtener los nombres completos de un array de personas
const obtenerNombresCompletos = (personas) => {
  return personas.map((persona) => {
    return obtenerNombreCompleto(persona);
  });
};

// Función para obtener las edades de un array de personas
const obtenerEdades = (personas) => {
  return personas.map((persona) => {
    return obtenerEdad(persona);
  });
};

// Función para obtener las ciudades de un array de personas
const obtenerCiudades = (personas) => {
  return personas.map((persona) => {
    return obtenerCiudad(persona);
  });
};

// Función principal
const main = () => {
  // Creamos un array con los datos de varias personas
  const personas = [
    { nombre: "Juan", apellido: "Pérez", edad: 25, ciudad: "Madrid" },
    { nombre: "Ana", apellido: "García", edad: 30, ciudad: "Barcelona" },
    { nombre: "Pedro", apellido: "López", edad: 35, ciudad: "Valencia" },
  ];

  // Obtenemos los nombres completos de las personas
  const nombresCompletos = obtenerNombresCompletos(personas);

  // Obtenemos las edades de las personas
  const edades = obtenerEdades(personas);

  // Obtenemos las ciudades de las personas
  const ciudades = obtenerCiudades(personas);

  // Imprimimos los resultados
  console.log("Nombres completos:");
  console.log(nombresCompletos);

  console.log("Edades:");
  console.log(edades);

  console.log("Ciudades:");
  console.log(ciudades);
};

// Llamamos a la función principal
main();
```

**Explicación del código:**

* La función `crearPersona` crea un objeto con los datos de una persona.
* La función `obtenerNombreCompleto` obtiene el nombre completo de una persona.
* La función `obtenerEdad` obtiene la edad de una persona.
* La función `obtenerCiudad` obtiene la ciudad de una persona.
* La función `crearArrayPersonas` crea un array con los datos de varias personas.
* La función `obtenerNombresCompletos` obtiene los nombres completos de un array de personas.
* La función `obtenerEdades` obtiene las edades de un array de personas.
* La función `obtenerCiudades` obtiene las ciudades de un array de personas.
* La función `main` es la función principal del programa y llama a las demás funciones para obtener los resultados.

El código crea un array con los datos de varias personas y luego utiliza las funciones para obtener los nombres completos, las edades y las ciudades de las personas. Finalmente, imprime los resultados en la consola.