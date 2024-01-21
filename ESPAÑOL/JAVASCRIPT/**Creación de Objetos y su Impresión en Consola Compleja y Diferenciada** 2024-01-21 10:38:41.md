```javascript
// Función para crear un objeto Persona con nombre, edad y género.
function Persona(nombre, edad, genero) {
  this.nombre = nombre;
  this.edad = edad;
  this.genero = genero;
}

// Función para crear un objeto Coche con marca, modelo y año.
function Coche(marca, modelo, año) {
  this.marca = marca;
  this.modelo = modelo;
  this.año = año;
}

// Función para crear un objeto Casa con dirección, tamaño y número de habitaciones.
function Casa(direccion, tamaño, habitaciones) {
  this.direccion = direccion;
  this.tamaño = tamaño;
  this.habitaciones = habitaciones;
}

// Array de objetos Persona.
const personas = [
  new Persona("Juan", 25, "Masculino"),
  new Persona("María", 30, "Femenino"),
  new Persona("Pedro", 40, "Masculino"),
  new Persona("Ana", 35, "Femenino"),
  new Persona("Luis", 20, "Masculino"),
];

// Array de objetos Coche.
const coches = [
  new Coche("Toyota", "Yaris", 2018),
  new Coche("Honda", "Civic", 2019),
  new Coche("Nissan", "Sentra", 2020),
  new Coche("Kia", "Rio", 2021),
  new Coche("Hyundai", "Accent", 2022),
];

// Array de objetos Casa.
const casas = [
  new Casa("Calle Mayor 123", "100 m2", 3),
  new Casa("Avenida Principal 456", "150 m2", 4),
  new Casa("Plaza España 789", "200 m2", 5),
  new Casa("Calle Real 1011", "250 m2", 6),
  new Casa("Plaza de la Constitución 1213", "300 m2", 7),
];

// Función para imprimir en consola el nombre de cada persona.
function imprimirPersonas() {
  console.log("Nombres de las personas:");
  for (let i = 0; i < personas.length; i++) {
    console.log(personas[i].nombre);
  }
}

// Función para imprimir en consola la marca y modelo de cada coche.
function imprimirCoches() {
  console.log("Marcas y modelos de los coches:");
  for (let i = 0; i < coches.length; i++) {
    console.log(`${coches[i].marca} ${coches[i].modelo}`);
  }
}

// Función para imprimir en consola la dirección y el tamaño de cada casa.
function imprimirCasas() {
  console.log("Direcciones y tamaños de las casas:");
  for (let i = 0; i < casas.length; i++) {
    console.log(`${casas[i].direccion} (${casas[i].tamaño})`);
  }
}

// Llamada a las funciones para imprimir los datos en consola.
imprimirPersonas();
imprimirCoches();
imprimirCasas();
```

Explicación del código:

* Se definen tres funciones constructoras: `Persona`, `Coche` y `Casa`, que crean objetos con propiedades específicas para cada tipo de objeto.
* Se definen tres arrays: `personas`, `coches` y `casas`, que contienen objetos creados con las funciones constructoras anteriores.
* Se definen tres funciones: `imprimirPersonas`, `imprimirCoches` e `imprimirCasas`, que imprimen en consola los datos de los objetos contenidos en los arrays correspondientes.
* Se realiza una llamada a cada una de las funciones para imprimir los datos en consola.

Este código es complejo y diferenciado porque:

* Utiliza funciones constructoras para crear objetos con propiedades específicas.
* Utiliza arrays para almacenar objetos de diferentes tipos.
* Utiliza funciones para imprimir en consola los datos de los objetos contenidos en los arrays.
* Es un código modular, ya que cada función tiene una responsabilidad específica.
* Es un código escalable, ya que se pueden añadir nuevos objetos a los arrays sin necesidad de modificar el código.