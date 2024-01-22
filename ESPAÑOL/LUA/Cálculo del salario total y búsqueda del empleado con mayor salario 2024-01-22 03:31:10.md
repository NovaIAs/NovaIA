```lua
-- Crear una tabla con los datos de los empleados
empleados = {
  {
    nombre = "Juan",
    apellido = "Perez",
    cargo = "Gerente",
    salario = 1000000
  },
  {
    nombre = "Ana",
    apellido = "Garcia",
    cargo = "Contadora",
    salario = 800000
  },
  {
    nombre = "Pedro",
    apellido = "Rodriguez",
    cargo = "Ingeniero",
    salario = 700000
  },
  {
    nombre = "Maria",
    apellido = "Fernandez",
    cargo = "Administradora",
    salario = 600000
  },
  {
    nombre = "Jose",
    apellido = "Lopez",
    cargo = "Vendedor",
    salario = 500000
  }
}

-- Definir una función para calcular el salario total de los empleados
function calcularSalarioTotal(empleados)
  local salarioTotal = 0

  -- Recorrer la tabla de empleados y sumar los salarios
  for i = 1, #empleados do
    salarioTotal = salarioTotal + empleados[i].salario
  end

  -- Retornar el salario total
  return salarioTotal
end

-- Calcular el salario total de los empleados
salarioTotal = calcularSalarioTotal(empleados)

-- Imprimir el salario total
print("El salario total de los empleados es: " .. salarioTotal)

-- Definir una función para encontrar al empleado con el mayor salario
function encontrarEmpleadoConMayorSalario(empleados)
  local empleadoConMayorSalario = empleados[1]

  -- Recorrer la tabla de empleados y buscar al empleado con el mayor salario
  for i = 2, #empleados do
    if empleados[i].salario > empleadoConMayorSalario.salario then
      empleadoConMayorSalario = empleados[i]
    end
  end

  -- Retornar el empleado con el mayor salario
  return empleadoConMayorSalario
end

-- Encontrar al empleado con el mayor salario
empleadoConMayorSalario = encontrarEmpleadoConMayorSalario(empleados)

-- Imprimir el nombre del empleado con el mayor salario
print("El empleado con el mayor salario es: " .. empleadoConMayorSalario.nombre .. " " .. empleadoConMayorSalario.apellido)

```

**Explicación del código:**

1. Creamos una tabla llamada `empleados` que contiene los datos de los empleados, como su nombre, apellido, cargo y salario.
2. Definimos una función llamada `calcularSalarioTotal()` que calcula el salario total de los empleados. La función recorre la tabla de empleados y suma los salarios de cada empleado.
3. Calculamos el salario total de los empleados llamando a la función `calcularSalarioTotal()`.
4. Imprimimos el salario total de los empleados.
5. Definimos una función llamada `encontrarEmpleadoConMayorSalario()` que busca al empleado con el mayor salario. La función recorre la tabla de empleados y compara el salario de cada empleado con el salario del empleado anterior. El empleado con el mayor salario es el que tiene el salario más alto.
6. Encontramos al empleado con el mayor salario llamando a la función `encontrarEmpleadoConMayorSalario()`.
7. Imprimimos el nombre del empleado con el mayor salario.