```javascript
// Crear un objeto que almacene los datos de un estudiante.
const estudiante = {
  nombre: "Juan",
  apellido: "Perez",
  edad: 20,
  matricula: 123456,
  notas: [
    {
      materia: "Matemáticas",
      nota: 85,
    },
    {
      materia: "Física",
      nota: 90,
    },
    {
      materia: "Química",
      nota: 75,
    },
  ],
};

// Definir una función que calcule el promedio de las notas de un estudiante.
function calcularPromedio(notas) {
  let sumaNotas = 0;
  for (let i = 0; i < notas.length; i++) {
    sumaNotas += notas[i].nota;
  }
  return sumaNotas / notas.length;
}

// Imprimir el promedio de las notas del estudiante.
console.log(`El promedio de las notas del estudiante es: ${calcularPromedio(estudiante.notas)}`);

// Definir una función que devuelva el nombre completo del estudiante.
function getNombreCompleto(estudiante) {
  return `${estudiante.nombre} ${estudiante.apellido}`;
}

// Imprimir el nombre completo del estudiante.
console.log(`El nombre completo del estudiante es: ${getNombreCompleto(estudiante)}`);

// Definir una función que devuelva la nota más alta del estudiante.
function getNotaMasAlta(notas) {
  let notaMasAlta = 0;
  for (let i = 0; i < notas.length; i++) {
    if (notas[i].nota > notaMasAlta) {
      notaMasAlta = notas[i].nota;
    }
  }
  return notaMasAlta;
}

// Imprimir la nota más alta del estudiante.
console.log(`La nota más alta del estudiante es: ${getNotaMasAlta(estudiante.notas)}`);

// Definir una función que devuelva la nota más baja del estudiante.
function getNotaMasBaja(notas) {
  let notaMasBaja = 100;
  for (let i = 0; i < notas.length; i++) {
    if (notas[i].nota < notaMasBaja) {
      notaMasBaja = notas[i].nota;
    }
  }
  return notaMasBaja;
}

// Imprimir la nota más baja del estudiante.
console.log(`La nota más baja del estudiante es: ${getNotaMasBaja(estudiante.notas)}`);

// Definir una función que devuelva las materias en las que el estudiante tiene una nota superior a 80.
function getMateriasConNotaSuperiorA80(notas) {
  const materiasConNotaSuperiorA80 = [];
  for (let i = 0; i < notas.length; i++) {
    if (notas[i].nota > 80) {
      materiasConNotaSuperiorA80.push(notas[i].materia);
    }
  }
  return materiasConNotaSuperiorA80;
}

// Imprimir las materias en las que el estudiante tiene una nota superior a 80.
console.log(`Las materias en las que el estudiante tiene una nota superior a 80 son: ${getMateriasConNotaSuperiorA80(estudiante.notas)}`);

// Definir una función que devuelva las materias en las que el estudiante tiene una nota inferior a 70.
function getMateriasConNotaInferiorA70(notas) {
  const materiasConNotaInferiorA70 = [];
  for (let i = 0; i < notas.length; i++) {
    if (notas[i].nota < 70) {
      materiasConNotaInferiorA70.push(notas[i].materia);
    }
  }
  return materiasConNotaInferiorA70;
}

// Imprimir las materias en las que el estudiante tiene una nota inferior a 70.
console.log(`Las materias en las que el estudiante tiene una nota inferior a 70 son: ${getMateriasConNotaInferiorA70(estudiante.notas)}`);

// Definir una función que devuelva un objeto con las estadísticas del estudiante.
function getEstadisticasEstudiante(estudiante) {
  const estadisticas = {
    promedio: calcularPromedio(estudiante.notas),
    nombreCompleto: getNombreCompleto(estudiante),
    notaMasAlta: getNotaMasAlta(estudiante.notas),
    notaMasBaja: getNotaMasBaja(estudiante.notas),
    materiasConNotaSuperiorA80: getMateriasConNotaSuperiorA80(estudiante.notas),