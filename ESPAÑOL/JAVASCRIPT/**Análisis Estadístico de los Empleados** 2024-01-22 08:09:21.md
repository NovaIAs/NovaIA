```javascript
// Definir una función para generar un número aleatorio entre dos límites.
function generarAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Crear un array de objetos con datos de empleados.
const empleados = [
  {
    nombre: "Juan",
    apellido: "Pérez",
    edad: 30,
    salario: 2000,
  },
  {
    nombre: "María",
    apellido: "López",
    edad: 25,
    salario: 1800,
  },
  {
    nombre: "Pedro",
    apellido: "García",
    edad: 35,
    salario: 2200,
  },
  {
    nombre: "Ana",
    apellido: "Fernández",
    edad: 28,
    salario: 1900,
  },
  {
    nombre: "José",
    apellido: "Rodríguez",
    edad: 32,
    salario: 2100,
  },
];

// Crear una función para ordenar el array de empleados por edad en orden ascendente.
function ordenarPorEdad(a, b) {
  return a.edad - b.edad;
}

// Ordenar el array de empleados por edad.
empleados.sort(ordenarPorEdad);

// Crear una función para generar un objeto con las estadísticas de los empleados.
function generarEstadisticas(empleados) {
  // Obtener el número total de empleados.
  const totalEmpleados = empleados.length;

  // Obtener la edad promedio de los empleados.
  let edadPromedio = 0;
  for (const empleado of empleados) {
    edadPromedio += empleado.edad;
  }
  edadPromedio /= totalEmpleados;

  // Obtener el salario promedio de los empleados.
  let salarioPromedio = 0;
  for (const empleado of empleados) {
    salarioPromedio += empleado.salario;
  }
  salarioPromedio /= totalEmpleados;

  // Obtener el empleado con mayor edad.
  let empleadoMayorEdad = empleados[0];
  for (const empleado of empleados) {
    if (empleado.edad > empleadoMayorEdad.edad) {
      empleadoMayorEdad = empleado;
    }
  }

  // Obtener el empleado con menor edad.
  let empleadoMenorEdad = empleados[0];
  for (const empleado of empleados) {
    if (empleado.edad < empleadoMenorEdad.edad) {
      empleadoMenorEdad = empleado;
    }
  }

  // Obtener el empleado con mayor salario.
  let empleadoMayorSalario = empleados[0];
  for (const empleado of empleados) {
    if (empleado.salario > empleadoMayorSalario.salario) {
      empleadoMayorSalario = empleado;
    }
  }

  // Obtener el empleado con menor salario.
  let empleadoMenorSalario = empleados[0];
  for (const empleado of empleados) {
    if (empleado.salario < empleadoMenorSalario.salario) {
      empleadoMenorSalario = empleado;
    }
  }

  // Crear un objeto con las estadísticas.
  const estadisticas = {
    totalEmpleados,
    edadPromedio,
    salarioPromedio,
    empleadoMayorEdad,
    empleadoMenorEdad,
    empleadoMayorSalario,
    empleadoMenorSalario,
  };

  // Devolver las estadísticas.
  return estadisticas;
}

// Generar las estadísticas de los empleados.
const estadisticas = generarEstadisticas(empleados);

// Mostrar las estadísticas de los empleados en la consola.
console.log("Total de empleados:", estadisticas.totalEmpleados);
console.log("Edad promedio:", estadisticas.edadPromedio);
console.log("Salario promedio:", estadisticas.salarioPromedio);
console.log("Empleado mayor edad:", estadisticas.empleadoMayorEdad);
console.log("Empleado menor edad:", estadisticas.empleadoMenorEdad);
console.log("Empleado mayor salario:", estadisticas.empleadoMayorSalario);
console.log("Empleado menor salario:", estadisticas.empleadoMenorSalario);
```

Explicación del código:

* La función `generarAleatorio()` genera un número aleatorio entre dos límites.
* La función `ordenarPorEdad()` ordena un array de objetos por edad en orden ascendente.
* La función `generarEstadisticas()` genera un objeto con las estadísticas de un array de empleados.
* El código principal ordena el array de empleados por edad y luego genera las estadísticas de los empleados.
* Finalmente, se muestran las estadísticas de los empleados en la consola.