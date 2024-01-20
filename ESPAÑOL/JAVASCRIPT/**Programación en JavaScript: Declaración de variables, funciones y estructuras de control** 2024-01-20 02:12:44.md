```javascript
// Declaración de variables
var nombre = "Juan";
var edad = 25;
var esEstudiante = true;
var notas = [8, 9, 10];
var materias = ["Matemáticas", "Física", "Química"];

// Función para saludar
function saludar(nombre) {
  console.log("Hola, " + nombre + "!");
}

// Función para calcular el promedio de las notas
function promedio(notas) {
  var suma = 0;
  for (var i = 0; i < notas.length; i++) {
    suma += notas[i];
  }
  return suma / notas.length;
}

// Función para imprimir las materias del estudiante
function imprimirMaterias(materias) {
  console.log("Materias:");
  for (var i = 0; i < materias.length; i++) {
    console.log(materias[i]);
  }
}

// Función principal
function main() {
  // Invocar la función para saludar al usuario
  saludar(nombre);

  // Invocar la función para calcular el promedio de las notas
  var promedioNotas = promedio(notas);
  console.log("Promedio de las notas:", promedioNotas);

  // Invocar la función para imprimir las materias del estudiante
  imprimirMaterias(materias);

  // Condicional para verificar si el estudiante es menor de edad
  if (edad < 18) {
    console.log(nombre + " es menor de edad.");
  } else {
    console.log(nombre + " es mayor de edad.");
  }

  // Bucle for para recorrer el arreglo de notas
  for (var i = 0; i < notas.length; i++) {
    console.log("Nota " + (i + 1) + ": " + notas[i]);
  }

  // Bucle while para imprimir los números del 1 al 10
  var contador = 1;
  while (contador <= 10) {
    console.log(contador);
    contador++;
  }
}

// Invocar la función principal
main();

```

Explicación del código:

* **Declaración de variables:** Se declaran varias variables, incluyendo el nombre del estudiante, su edad, su estado de estudiante, sus notas y sus materias.
* **Función para saludar:** Se define una función llamada `saludar` que recibe un nombre como argumento y lo imprime en la consola.
* **Función para calcular el promedio de las notas:** Se define una función llamada `promedio` que recibe un arreglo de notas como argumento y devuelve el promedio de las notas.
* **Función para imprimir las materias del estudiante:** Se define una función llamada `imprimirMaterias` que recibe un arreglo de materias como argumento e imprime cada materia en la consola.
* **Función principal:** Se define una función llamada `main` que es el punto de entrada del programa. Esta función invoca a las demás funciones y realiza varias operaciones, como imprimir el nombre del estudiante, calcular el promedio de las notas, imprimir las materias del estudiante, verificar si el estudiante es menor de edad, recorrer el arreglo de notas y utilizar un bucle while para imprimir los números del 1 al 10.
* **Invocar la función principal:** Se invoca la función `main` para ejecutar el programa.