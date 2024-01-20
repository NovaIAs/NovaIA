```javascript
// Definir la estructura de la casa
const casa = {
  habitaciones: [
    {
      nombre: 'Sala',
      muebles: ['Sofá', 'Sillón', 'Televisor']
    },
    {
      nombre: 'Cocina',
      muebles: ['Refrigerador', 'Cocina', 'Microondas']
    },
    {
      nombre: 'Habitación',
      muebles: ['Cama', 'Armario', 'Escritorio']
    }
  ],
  baño: {
    muebles: ['Inodoro', 'Lavabo', 'Ducha']
  },
  garaje: {
    muebles: ['Coche', 'Bicicleta', 'Herramientas']
  }
};

// Definir la función para mostrar la información de la casa
const mostrarCasa = () => {
  // Recorrer las habitaciones de la casa
  for (const habitacion of casa.habitaciones) {
    // Mostrar el nombre de la habitación
    console.log(`Habitación: ${habitacion.nombre}`);

    // Recorrer los muebles de la habitación
    for (const mueble of habitacion.muebles) {
      // Mostrar el nombre del mueble
      console.log(`Mueble: ${mueble}`);
    }

    // Mostrar una línea en blanco para separar las habitaciones
    console.log('');
  }

  // Mostrar el baño
  console.log('Baño:');

  // Recorrer los muebles del baño
  for (const mueble of casa.baño.muebles) {
    // Mostrar el nombre del mueble
    console.log(`Mueble: ${mueble}`);
  }

  // Mostrar una línea en blanco para separar el baño del garaje
  console.log('');

  // Mostrar el garaje
  console.log('Garaje:');

  // Recorrer los muebles del garaje
  for (const mueble of casa.garaje.muebles) {
    // Mostrar el nombre del mueble
    console.log(`Mueble: ${mueble}`);
  }

  // Mostrar una línea en blanco para separar el garaje del final de la casa
  console.log('');
};

// Invocar la función para mostrar la información de la casa
mostrarCasa();
```

Explicación del código:

* Se define una estructura de datos que representa la casa, con sus habitaciones, baño y garaje, y los muebles que se encuentran en cada uno de ellos.
* Se define una función para mostrar la información de la casa, que recorre las habitaciones, el baño y el garaje, y muestra el nombre de cada habitación y de los muebles que se encuentran en ella.
* Se invoca la función para mostrar la información de la casa, lo que imprime en la consola la información de todas las habitaciones, el baño y el garaje de la casa.

El código es complejo ya que utiliza estructuras de datos anidadas (la casa tiene habitaciones, baño y garaje, y cada habitación y el baño tienen muebles), y utiliza bucles para recorrer estas estructuras de datos y mostrar la información de la casa.