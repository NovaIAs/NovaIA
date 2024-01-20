```typescript
// Objeto Complejo para representar una Persona
type Persona = {
  nombre: string,
  apellido: string,
  edad: number,
  profesion: string[],
  hobbies: string[],
  direccion: {
    calle: string,
    numero: number,
    ciudad: string,
    pais: string
  },
  contacto: {
    telefono: string,
    correoElectronico: string,
    redesSociales: {
      facebook: string,
      twitter: string,
      instagram: string
    }
  }
};

// Función para crear una nueva Persona
const crearPersona = (nombre: string, apellido: string, edad: number, profesion: string[], hobbies: string[], calle: string, numero: number, ciudad: string, pais: string, telefono: string, correoElectronico: string, facebook: string, twitter: string, instagram: string): Persona => {
  // Creación de un nuevo objeto Persona con los datos proporcionados
  return {
    nombre: nombre,
    apellido: apellido,
    edad: edad,
    profesion: profesion,
    hobbies: hobbies,
    direccion: {
      calle: calle,
      numero: numero,
      ciudad: ciudad,
      pais: pais
    },
    contacto: {
      telefono: telefono,
      correoElectronico: correoElectronico,
      redesSociales: {
        facebook: facebook,
        twitter: twitter,
        instagram: instagram
      }
    }
  };
};

// Función para imprimir los datos de una Persona
const imprimirPersona = (persona: Persona) => {
  // Impresión de los datos de la persona en un formato legible
  console.log(`\nNombre: ${persona.nombre} ${persona.apellido}`);
  console.log(`Edad: ${persona.edad}`);
  console.log(`Profesión: ${persona.profesion.join(", ")}`);
  console.log(`Hobbies: ${persona.hobbies.join(", ")}`);
  console.log(`Dirección: ${persona.direccion.calle} ${persona.direccion.numero}, ${persona.direccion.ciudad}, ${persona.direccion.pais}`);
  console.log(`Teléfono: ${persona.contacto.telefono}`);
  console.log(`Correo electrónico: ${persona.contacto.correoElectronico}`);
  console.log(`Redes sociales:`);
  console.log(`  - Facebook: ${persona.contacto.redesSociales.facebook}`);
  console.log(`  - Twitter: ${persona.contacto.redesSociales.twitter}`);
  console.log(`  - Instagram: ${persona.contacto.redesSociales.instagram}`);
};

// Creación de una nueva Persona utilizando la función crearPersona()
const persona1 = crearPersona("Juan", "García", 30, ["Ingeniero de software", "Emprendedor"], ["Viajar", "Leer", "Programar"], "Calle Mayor", 123, "Madrid", "España", "123-456-7890", "juan.garcia@ejemplo.com", "juan.garcia", "juangarcia", "juangarciaphoto");

// Impresión de los datos de la persona creada
imprimirPersona(persona1);
```

Explicación del código:

* **Objeto Complejo Persona:** Se crea un tipo personalizado llamado `Persona` utilizando la sintaxis de TypeScript. Este objeto representa a una persona y contiene información como nombre, apellido, edad, profesión, aficiones, dirección y datos de contacto.
* **Función crearPersona:** Se crea una función llamada `crearPersona` que toma varios argumentos para crear un nuevo objeto Persona. Estos argumentos incluyen el nombre, apellido, edad, profesión, aficiones, dirección y datos de contacto de la persona.
* **Función imprimirPersona:** Se crea una función llamada `imprimirPersona` que toma un objeto Persona como argumento e imprime sus datos en un formato legible.
* **Instanciación del Objeto Persona:** Se utiliza la función `crearPersona` para crear una nueva Persona llamada `persona1` pasándole los valores correspondientes.
* **Impresión de los Datos de la Persona:** Por último, se llama a la función `imprimirPersona` para imprimir los datos de la persona `persona1` en la consola.