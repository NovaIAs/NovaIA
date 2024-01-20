```php
<?php

// Definir la función principal
function main() {
  // Crear un array asociativo con los datos de los empleados
  $empleados = array(
    array("nombre" => "Juan", "apellido" => "Pérez", "edad" => 25, "salario" => 1000),
    array("nombre" => "María", "apellido" => "García", "edad" => 30, "salario" => 1200),
    array("nombre" => "Pedro", "apellido" => "López", "edad" => 35, "salario" => 1400)
  );

  // Crear una función para calcular el salario promedio de los empleados
  function calcularSalarioPromedio($empleados) {
    // Inicializar la variable que almacenará el salario promedio
    $salarioPromedio = 0;

    // Recorrer el array de empleados
    foreach ($empleados as $empleado) {
      // Sumar el salario de cada empleado al salario promedio
      $salarioPromedio += $empleado["salario"];
    }

    // Dividir el salario promedio por el número de empleados para obtener el salario promedio
    $salarioPromedio /= count($empleados);

    // Devolver el salario promedio
    return $salarioPromedio;
  }

  // Calcular el salario promedio de los empleados
  $salarioPromedio = calcularSalarioPromedio($empleados);

  // Crear una función para imprimir los datos de los empleados
  function imprimirDatosEmpleados($empleados) {
    // Recorrer el array de empleados
    foreach ($empleados as $empleado) {
      // Imprimir el nombre, el apellido, la edad y el salario de cada empleado
      echo "Nombre: " . $empleado["nombre"] . "<br>";
      echo "Apellido: " . $empleado["apellido"] . "<br>";
      echo "Edad: " . $empleado["edad"] . "<br>";
      echo "Salario: " . $empleado["salario"] . "<br><br>";
    }
  }

  // Imprimir los datos de los empleados
  imprimirDatosEmpleados($empleados);

  // Imprimir el salario promedio de los empleados
  echo "Salario promedio: " . $salarioPromedio;
}

// Llamar a la función principal
main();

?>
```

Explicación del código:

* La función `main()` se define como el punto de entrada del programa.
* Se crea un array asociativo con los datos de los empleados.
* Se crea una función `calcularSalarioPromedio()` para calcular el salario promedio de los empleados.
* Se llama a la función `calcularSalarioPromedio()` para obtener el salario promedio de los empleados.
* Se crea una función `imprimirDatosEmpleados()` para imprimir los datos de los empleados.
* Se llama a la función `imprimirDatosEmpleados()` para imprimir los datos de los empleados.
* Se imprime el salario promedio de los empleados.