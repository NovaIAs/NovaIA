```javascript
// Objeto `Coche` con sus propiedades y métodos
class Coche {
  constructor(marca, modelo, año) {
    this.marca = marca;
    this.modelo = modelo;
    this.año = año;
    this._velocidad = 0; // Propiedad privada
  }

  // Método para obtener la velocidad del coche
  getVelocidad() {
    return this._velocidad;
  }

  // Método para acelerar el coche
  acelerar(cantidad) {
    this._velocidad += cantidad;
  }

  // Método para frenar el coche
  frenar(cantidad) {
    this._velocidad -= cantidad;
    if (this._velocidad < 0) {
      this._velocidad = 0;
    }
  }

  // Método para mostrar información del coche
  mostrarInformacion() {
    console.log(`Marca: ${this.marca}`);
    console.log(`Modelo: ${this.modelo}`);
    console.log(`Año: ${this.año}`);
    console.log(`Velocidad: ${this._velocidad}`);
  }
}

// Función para crear un coche
function crearCoche(marca, modelo, año) {
  const coche = new Coche(marca, modelo, año);
  return coche;
}

// Función para iniciar una carrera
function iniciarCarrera(coches) {
  // Mostrar información inicial de los coches
  console.log("Información inicial de los coches:");
  coches.forEach((coche) => {
    coche.mostrarInformacion();
  });

  // Iniciar la carrera
  console.log("\nCarrera iniciada!");

  // Simular la carrera
  for (let i = 0; i < 10; i++) {
    // Acelerar o frenar los coches aleatoriamente
    coches.forEach((coche) => {
      const accion = Math.random() < 0.5 ? "acelerar" : "frenar";
      const cantidad = Math.floor(Math.random() * 10) + 1;
      accion === "acelerar" ? coche.acelerar(cantidad) : coche.frenar(cantidad);
    });

    // Mostrar la información de los coches en cada vuelta
    console.log(`\nVuelta ${i + 1}`);
    coches.forEach((coche) => {
      coche.mostrarInformacion();
    });
  }

  // Mostrar el resultado de la carrera
  console.log("\nResultado de la carrera:");
  coches.sort((a, b) => b.getVelocidad() - a.getVelocidad()); // Ordenar los coches por velocidad
  coches.forEach((coche, idx) => {
    console.log(`${idx + 1}° lugar: ${coche.marca} ${coche.modelo} (${coche.getVelocidad()} km/h)`);
  });
}

// Crear los coches
const coche1 = crearCoche("Toyota", "Corolla", 2020);
const coche2 = crearCoche("Honda", "Civic", 2019);
const coche3 = crearCoche("BMW", "Serie 3", 2021);

// Iniciar la carrera
iniciarCarrera([coche1, coche2, coche3]);
```

Explicación:

Este código simula una carrera de coches. Crea objetos `Coche` con propiedades como marca, modelo y año, y métodos para acelerar, frenar y mostrar información del coche.

La función `crearCoche()` crea un nuevo objeto `Coche` con los parámetros especificados.

La función `iniciarCarrera()` toma un array de objetos `Coche` como argumento y simula la carrera. Muestra la información inicial de los coches, inicia la carrera simulando aceleraciones y frenadas aleatorias, y muestra la información de los coches en cada vuelta. Finalmente, muestra el resultado de la carrera ordenando los coches por velocidad.

El código utiliza funciones de orden superior como `forEach()` y `sort()` para procesar los objetos `Coche`. También utiliza cierres para mantener el estado de las variables dentro de las funciones.