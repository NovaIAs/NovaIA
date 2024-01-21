```javascript
// Definición de datos
const datos = [
  { nombre: "Juan", edad: 25, ciudad: "Madrid" },
  { nombre: "María", edad: 30, ciudad: "Barcelona" },
  { nombre: "Pedro", edad: 35, ciudad: "Valencia" },
  { nombre: "Ana", edad: 40, ciudad: "Sevilla" },
  { nombre: "José", edad: 45, ciudad: "Zaragoza" },
];

// Función para filtrar los datos por una determinada ciudad
const filtrarPorCiudad = (ciudad) => {
  return datos.filter((persona) => persona.ciudad === ciudad);
};

// Función para obtener el promedio de edad de las personas de una determinada ciudad
const obtenerPromedioEdad = (ciudad) => {
  const personas = filtrarPorCiudad(ciudad);
  const sumaEdades = personas.reduce((acumulador, persona) => acumulador + persona.edad, 0);
  return sumaEdades / personas.length;
};

// Función para obtener la persona más joven de una determinada ciudad
const obtenerPersonaMasJoven = (ciudad) => {
  const personas = filtrarPorCiudad(ciudad);
  return personas.reduce((personaMasJoven, persona) => {
    if (persona.edad < personaMasJoven.edad) {
      return persona;
    }
    return personaMasJoven;
  }, personas[0]);
};

// Función para obtener la persona más mayor de una determinada ciudad
const obtenerPersonaMasMayor = (ciudad) => {
  const personas = filtrarPorCiudad(ciudad);
  return personas.reduce((personaMasMayor, persona) => {
    if (persona.edad > personaMasMayor.edad) {
      return persona;
    }
    return personaMasMayor;
  }, personas[0]);
};

// Función para mostrar los resultados en una página web
const mostrarResultados = () => {
  const resultados = document.getElementById("resultados");

  // Obtener los datos de la ciudad seleccionada
  const ciudad = document.getElementById("ciudad").value;

  // Filtrar los datos por la ciudad seleccionada
  const personas = filtrarPorCiudad(ciudad);

  // Mostrar los datos de las personas en la página web
  let html = "";
  personas.forEach((persona) => {
    html += `<li>${persona.nombre} - ${persona.edad} años - ${persona.ciudad}</li>`;
  });
  resultados.innerHTML = html;

  // Obtener el promedio de edad de las personas de la ciudad seleccionada
  const promedioEdad = obtenerPromedioEdad(ciudad);

  // Mostrar el promedio de edad en la página web
  resultados.innerHTML += `<p>Promedio de edad: ${promedioEdad} años</p>`;

  // Obtener la persona más joven de la ciudad seleccionada
  const personaMasJoven = obtenerPersonaMasJoven(ciudad);

  // Mostrar la persona más joven en la página web
  resultados.innerHTML += `<p>Persona más joven: ${personaMasJoven.nombre} - ${personaMasJoven.edad} años</p>`;

  // Obtener la persona más mayor de la ciudad seleccionada
  const personaMasMayor = obtenerPersonaMasMayor(ciudad);

  // Mostrar la persona más mayor en la página web
  resultados.innerHTML += `<p>Persona más mayor: ${personaMasMayor.nombre} - ${personaMasMayor.edad} años</p>`;
};

// Agregar un evento al botón para mostrar los resultados
const boton = document.getElementById("boton");
boton.addEventListener("click", mostrarResultados);
```
Este código JavaScript realiza un análisis de datos sobre una lista de personas, incluyendo operaciones de filtrado, cálculo de promedios, y búsqueda de valores extremos, y muestra los resultados en una página web.

Primero, el código define un conjunto de datos que incluye información sobre personas, como su nombre, edad y ciudad. Luego, se definen cuatro funciones que realizan diferentes operaciones sobre los datos:

* `filtrarPorCiudad`: Filtra los datos para obtener solo las personas que viven en una ciudad determinada.
* `obtenerPromedioEdad`: Calcula el promedio de edad de las personas que viven en una ciudad determinada.
* `obtenerPersonaMasJoven`: Obtiene la persona más joven que vive en una ciudad determinada.
* `obtenerPersonaMasMayor`: Obtiene la persona más mayor que vive en una ciudad determinada.

Finalmente, se define una función `mostrarResultados` que utiliza las funciones anteriores para obtener y mostrar los resultados del análisis de datos en una página web. La función se ejecuta cuando se hace clic en un botón, y muestra los datos en un elemento HTML con id "resultados".

Este código es un ejemplo de cómo utilizar JavaScript para realizar operaciones complejas sobre datos y mostrar los resultados en una interfaz de usuario.