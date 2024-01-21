```javascript
// Creación de un objeto "Persona" con propiedades y métodos
const Persona = {
  nombre: "Juan",
  apellido: "García",
  edad: 25,
  hablar: function() {
    console.log("Hola, mi nombre es " + this.nombre + " " + this.apellido + " y tengo " + this.edad + " años.");
  }
};

// Creación de un array de objetos "Persona"
const personas = [
  {
    nombre: "María",
    apellido: "López",
    edad: 30
  },
  {
    nombre: "Pedro",
    apellido: "Sánchez",
    edad: 35
  },
  {
    nombre: "Ana",
    apellido: "Fernández",
    edad: 40
  }
];

// Función para recorrer el array de personas y llamar al método "hablar" de cada una
const saludarPersonas = function() {
  for (let i = 0; i < personas.length; i++) {
    personas[i].hablar();
  }
};

// Función para crear un nuevo objeto "Persona" y agregarlo al array de personas
const agregarPersona = function(nombre, apellido, edad) {
  const nuevaPersona = {
    nombre: nombre,
    apellido: apellido,
    edad: edad
  };
  personas.push(nuevaPersona);
};

// Función para eliminar una persona del array de personas por su nombre
const eliminarPersona = function(nombre) {
  for (let i = 0; i < personas.length; i++) {
    if (personas[i].nombre === nombre) {
      personas.splice(i, 1);
      break;
    }
  }
};

// Función para buscar una persona en el array de personas por su nombre
const buscarPersona = function(nombre) {
  for (let i = 0; i < personas.length; i++) {
    if (personas[i].nombre === nombre) {
      return personas[i];
    }
  }
  return null;
};

// Función para ordenar el array de personas por edad en orden creciente
const ordenarPersonasPorEdad = function() {
  personas.sort((a, b) => a.edad - b.edad);
};

// Función para ordenar el array de personas por nombre en orden alfabético
const ordenarPersonasPorNombre = function() {
  personas.sort((a, b) => a.nombre.localeCompare(b.nombre));
};

// Función para mostrar el array de personas en una tabla HTML
const mostrarPersonasEnTabla = function() {
  let tabla = "<table><tr><th>Nombre</th><th>Apellido</th><th>Edad</th></tr>";
  for (let i = 0; i < personas.length; i++) {
    tabla += "<tr><td>" + personas[i].nombre + "</td><td>" + personas[i].apellido + "</td><td>" + personas[i].edad + "</td></tr>";
  }
  tabla += "</table>";
  document.getElementById("tabla-personas").innerHTML = tabla;
};

// Eventos de botones
document.getElementById("boton-saludar").addEventListener("click", saludarPersonas);
document.getElementById("boton-agregar").addEventListener("click", () => {
  const nombre = document.getElementById("nombre-nueva-persona").value;
  const apellido = document.getElementById("apellido-nueva-persona").value;
  const edad = document.getElementById("edad-nueva-persona").value;
  agregarPersona(nombre, apellido, edad);
  mostrarPersonasEnTabla();
});
document.getElementById("boton-eliminar").addEventListener("click", () => {
  const nombre = document.getElementById("nombre-persona-eliminar").value;
  eliminarPersona(nombre);
  mostrarPersonasEnTabla();
});
document.getElementById("boton-buscar").addEventListener("click", () => {
  const nombre = document.getElementById("nombre-persona-buscar").value;
  const persona = buscarPersona(nombre);
  if (persona) {
    alert("Persona encontrada: " + persona.nombre + " " + persona.apellido + ", edad: " + persona.edad);
  } else {
    alert("Persona no encontrada.");
  }
});
document.getElementById("boton-ordenar-edad").addEventListener("click", () => {
  ordenarPersonasPorEdad();
  mostrarPersonasEnTabla();
});
document.getElementById("boton-ordenar-nombre").addEventListener("click", () => {
  ordenarPersonasPorNombre();
  mostrarPersonasEnTabla();
});
```

El código anterior crea un objeto "Persona" con propiedades y métodos, un array de objetos "Persona", y varias funciones para interactuar con el array de personas. Incluye funciones para saludar a todas las personas, agregar una nueva persona, eliminar una persona, buscar una persona, ordenar el array de personas por edad o por nombre, y mostrar el array de personas en una tabla HTML. El código también incluye eventos de botones para interactuar con las funciones y mostrar los resultados en la página web.