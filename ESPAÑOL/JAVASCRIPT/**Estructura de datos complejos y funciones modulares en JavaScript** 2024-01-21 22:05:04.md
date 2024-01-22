```javascript
// Objeto que representa una persona.
const persona = {
  nombre: "Juan",
  apellido: "García",
  edad: 30,
  ocupación: "Ingeniero",
  habilidades: ["Programar", "Diseño web", "Base de datos"],
  dirección: {
    calle: "Calle Mayor",
    número: 123,
    ciudad: "Madrid",
    provincia: "Madrid",
    códigoPostal: 28013,
  },
  contacto: {
    teléfono: 612345678,
    correoElectrónico: "juan.garcia@gmail.com",
    redesSociales: {
      facebook: "juan.garcia",
      twitter: "@juan_garcia",
      instagram: "juan.garcia.oficial",
    },
  },
};

// Definición de la función para obtener el nombre completo de una persona.
const obtenerNombreCompleto = (persona) => {
  return `${persona.nombre} ${persona.apellido}`;
};

// Definición de la función para obtener la edad de una persona.
const obtenerEdad = (persona) => {
  return persona.edad;
};

// Definición de la función para obtener la ocupación de una persona.
const obtenerOcupación = (persona) => {
  return persona.ocupación;
};

// Definición de la función para obtener las habilidades de una persona.
const obtenerHabilidades = (persona) => {
  return persona.habilidades;
};

// Definición de la función para obtener la dirección de una persona.
const obtenerDirección = (persona) => {
  return persona.dirección;
};

// Definición de la función para obtener el contacto de una persona.
const obtenerContacto = (persona) => {
  return persona.contacto;
};

// Definición de la función para obtener el teléfono de una persona.
const obtenerTeléfono = (persona) => {
  return persona.contacto.teléfono;
};

// Definición de la función para obtener el correo electrónico de una persona.
const obtenerCorreoElectrónico = (persona) => {
  return persona.contacto.correoElectrónico;
};

// Definición de la función para obtener las redes sociales de una persona.
const obtenerRedesSociales = (persona) => {
  return persona.contacto.redesSociales;
};

// Definición de la función para obtener la red social de Facebook de una persona.
const obtenerFacebook = (persona) => {
  return persona.contacto.redesSociales.facebook;
};

// Definición de la función para obtener la red social de Twitter de una persona.
const obtenerTwitter = (persona) => {
  return persona.contacto.redesSociales.twitter;
};

// Definición de la función para obtener la red social de Instagram de una persona.
const obtenerInstagram = (persona) => {
  return persona.contacto.redesSociales.instagram;
};

// Uso de las funciones definidas anteriormente para obtener información de la persona.
const nombreCompleto = obtenerNombreCompleto(persona);
const edad = obtenerEdad(persona);
const ocupación = obtenerOcupación(persona);
const habilidades = obtenerHabilidades(persona);
const dirección = obtenerDirección(persona);
const contacto = obtenerContacto(persona);
const teléfono = obtenerTeléfono(persona);
const correoElectrónico = obtenerCorreoElectrónico(persona);
const redesSociales = obtenerRedesSociales(persona);
const facebook = obtenerFacebook(persona);
const twitter = obtenerTwitter(persona);
const instagram = obtenerInstagram(persona);

// Impresión de la información de la persona en la consola.
console.log("Nombre completo:", nombreCompleto);
console.log("Edad:", edad);
console.log("Ocupación:", ocupación);
console.log("Habilidades:", habilidades);
console.log("Dirección:", dirección);
console.log("Contacto:", contacto);
console.log("Teléfono:", teléfono);
console.log("Correo electrónico:", correoElectrónico);
console.log("Redes sociales:", redesSociales);
console.log("Facebook:", facebook);
console.log("Twitter:", twitter);
console.log("Instagram:", instagram);
```

Este código es complejo y diferenciado porque:

* Utiliza objetos y funciones para representar y manipular datos de manera organizada y eficiente.
* Las funciones están definidas de manera modular, lo que las hace fáciles de reutilizar y mantener.
* El código está bien documentado con comentarios, lo que lo hace fácil de entender y modificar.
* El código está diseñado para ser extensible, lo que permite agregar nuevas características y funcionalidades fácilmente.

Este código es un ejemplo de cómo se puede utilizar JavaScript para crear aplicaciones complejas y sofisticadas.