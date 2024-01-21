```javascript
// Definición de la función principal
function obtenerInformacionCompleja(datos) {
  // Comprobar si los datos son válidos
  if (!datos || typeof datos !== "object") {
    throw new Error("Se esperan datos válidos como objeto.");
  }

  // Inicializar variables
  let informacionCompleja = "";
  let contador = 0;

  // Recorrer el objeto de datos
  for (const propiedad in datos) {
    // Obtener el valor de la propiedad actual
    const valor = datos[propiedad];

    // Comprobar si la propiedad es de tipo objeto
    if (typeof valor === "object") {
      // Si es un objeto, llamar recursivamente a la función para obtener información
      const informacionRecursiva = obtenerInformacionCompleja(valor);

      // Concatenar la información recursiva a la información compleja
      informacionCompleja += `Propiedad: ${propiedad}, Información: ${informacionRecursiva}\n`;
    } else {
      // Si no es un objeto, concatenar la información directamente
      informacionCompleja += `Propiedad: ${propiedad}, Valor: ${valor}\n`;
    }

    // Incrementar el contador para llevar la cuenta de las propiedades procesadas
    contador++;
  }

  // Devolver la información compleja
  return `Información compleja (${contador} propiedades):\n${informacionCompleja}`;
}

// Ejemplo de uso de la función
const datos = {
  nombre: "Juan",
  apellido: "Pérez",
  edad: 25,
  direccion: {
    calle: "Calle Mayor",
    numero: 10,
    ciudad: "Madrid",
    pais: "España",
  },
  hobbies: ["Leer", "Viajar", "Cocinar"],
};

// Obtener la información compleja de los datos de ejemplo
const informacionCompleja = obtenerInformacionCompleja(datos);

// Mostrar la información compleja en la consola
console.log(informacionCompleja);
```

Explicación del código:

* La función `obtenerInformacionCompleja` toma como argumento un objeto de datos y devuelve una cadena de texto con la información compleja del objeto.
* La función comprueba primero si los datos son válidos, es decir, si son un objeto y no son `null` o `undefined`.
* Si los datos son válidos, la función inicializa dos variables: `informacionCompleja`, que almacenará la información compleja del objeto, y `contador`, que llevará la cuenta de las propiedades procesadas.
* La función recorre el objeto de datos utilizando un bucle `for...in`. Para cada propiedad del objeto, la función obtiene el valor de la propiedad y comprueba si es de tipo objeto.
* Si el valor es un objeto, la función llama recursivamente a sí misma para obtener la información compleja del objeto anidado. La información recursiva se concatena a la información compleja.
* Si el valor no es un objeto, la función concatena directamente la información de la propiedad a la información compleja.
* La función incrementa el contador para llevar la cuenta de las propiedades procesadas.
* Finalmente, la función devuelve la información compleja del objeto.

En el ejemplo de uso, se define un objeto de datos con información de una persona, incluyendo su nombre, apellido, edad, dirección y aficiones. A continuación, se llama a la función `obtenerInformacionCompleja` con este objeto como argumento y se muestra la información compleja en la consola.