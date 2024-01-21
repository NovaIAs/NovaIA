```javascript
// Crear un objeto llamado "persona" con las propiedades "nombre", "apellido", "edad" y "ocupación".
const persona = {
  nombre: "Juan",
  apellido: "Pérez",
  edad: 30,
  ocupación: "Ingeniero de software"
};

// Crear una función llamada "mostrarPersona" que tome un objeto de persona como argumento y muestre sus propiedades.
function mostrarPersona(persona) {
  console.log(`Nombre: ${persona.nombre}`);
  console.log(`Apellido: ${persona.apellido}`);
  console.log(`Edad: ${persona.edad}`);
  console.log(`Ocupación: ${persona.ocupación}`);
}

// Crear un array llamado "personas" con tres objetos de persona.
const personas = [
  {
    nombre: "Ana",
    apellido: "García",
    edad: 25,
    ocupación: "Médico"
  },
  {
    nombre: "Luis",
    apellido: "Suárez",
    edad: 35,
    ocupación: "Abogado"
  },
  {
    nombre: "María",
    apellido: "Rodríguez",
    edad: 40,
    ocupación: "Profesora"
  }
];

// Iterar sobre el array "personas" y mostrar cada objeto de persona usando la función "mostrarPersona".
for (let persona of personas) {
  mostrarPersona(persona);
}

// Crear un formulario HTML con campos para ingresar el nombre, apellido, edad y ocupación de una persona.
<!DOCTYPE html>
<html>
<head>
  <title>Formulario de persona</title>
</head>
<body>
  <h1>Formulario de persona</h1>
  <form>
    <label for="nombre">Nombre:</label>
    <input type="text" id="nombre">
    <br>
    <label for="apellido">Apellido:</label>
    <input type="text" id="apellido">
    <br>
    <label for="edad">Edad:</label>
    <input type="number" id="edad">
    <br>
    <label for="ocupación">Ocupación:</label>
    <input type="text" id="ocupación">
    <br>
    <input type="submit" value="Enviar">
  </form>
</body>
</html>

// Crear una función JavaScript que se ejecute cuando se envíe el formulario.
<script>
document.querySelector("form").addEventListener("submit", (event) => {
  event.preventDefault();

  // Obtener los valores de los campos del formulario.
  const nombre = document.querySelector("#nombre").value;
  const apellido = document.querySelector("#apellido").value;
  const edad = document.querySelector("#edad").value;
  const ocupación = document.querySelector("#ocupación").value;

  // Crear un objeto de persona con los valores obtenidos.
  const persona = {
    nombre: nombre,
    apellido: apellido,
    edad: edad,
    ocupación: ocupación
  };

  // Mostrar el objeto de persona usando la función "mostrarPersona".
  mostrarPersona(persona);
});
</script>

// Este código crea un objeto de persona, una función para mostrar las propiedades de un objeto de persona, un array de objetos de persona, un formulario HTML para ingresar los datos de una persona y una función JavaScript para manejar el envío del formulario.