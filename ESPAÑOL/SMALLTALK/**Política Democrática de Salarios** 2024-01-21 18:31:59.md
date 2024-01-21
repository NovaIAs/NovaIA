```smalltalk
(Política Democrática de Salarios)

**Introducción**

Este código en Smalltalk crea un programa que implementa una política democrática de salarios. La política democrática de salarios es un sistema en el que los trabajadores de una empresa deciden colectivamente la distribución de los salarios. El programa permite a los trabajadores votar sobre diferentes propuestas de distribución de salarios, y la propuesta que reciba más votos se implementa.

**Código**

```smalltalk
(Política Democrática de Salarios)

**Clase Empleado**

```smalltalk
Clase Empleado
	instancia variable nombre
	instancia variable salario

	inicializarNombre: unNombre
		nombre <- unNombre
	

	inicializarSalario: unSalario
		salario <- unSalario
	

	getNombre
		^nombre
	

	getSalario
		^salario
	

	toString
		^"Empleado: ", nombre, " Salario: ", salario
```

**Clase Votante**

```smalltalk
Clase Votante
	instancia variable empleado
	instancia variable votos

	inicializar: unEmpleado
		empleado <- unEmpleado
		votos <- Dictionary new
	

	getEmpleado
		^empleado
	

	getVotos
		^votos
	

	votar: unaPropuesta
		votos at: unaPropuesta put: votos at: unaPropuesta + 1
	

	toString
		^"Votante: ", empleado, " Votos: ", votos
```

**Clase Propuesta**

```smalltalk
Clase Propuesta
	instancia variable nombre
	instancia variable salarios

	inicializarNombre: unNombre
		nombre <- unNombre
		salarios <- Dictionary new
	

	inicializarSalarios: unosSalarios
		salarios <- unosSalarios
	

	getNombre
		^nombre
	

	getSalarios
		^salarios
	

	toString
		^"Propuesta: ", nombre, " Salarios: ", salarios
```

**Clase Empresa**

```smalltalk
Clase Empresa
	instancia variable empleados
	instancia variable votantes
	instancia variable propuestas
	instancia variable presupuesto

	inicializar: unosEmpleados
		empleados <- unosEmpleados
		votantes <- Dictionary new
		propuestas <- Dictionary new
		presupuesto <- 0
	

	getEmpleados
		^empleados
	

	getVotantes
		^votantes
	

	getPropuestas
		^propuestas
	

	getPresupuesto
		^presupuesto
	

	setPresupuesto: unPresupuesto
		presupuesto <- unPresupuesto
	

	añadirEmpleado: unEmpleado
		empleados add: unEmpleado
	

	añadirVotante: unVotante
		votantes at: unEmpleado put: unVotante
	

	añadirPropuesta: unaPropuesta
		propuestas at: unaPropuesta getNombre put: unaPropuesta
	

	votar: unaPropuesta
		votantes valuesDo: [:votante | votante votar: unaPropuesta]
	

	calcularSalarios
		propuestaGanadora <- propuestas values detect: [:propuesta | propuesta getVotos > (propuestas size / 2)]
		salarios <- propuestaGanadora getSalarios
		salarios keys do: [:empleado |
			empleado setSalario: salarios at: empleado]
	

	pagarSalarios
		salarios keys do: [:empleado |
			empleado getSalario printString; cr]
	

	toString
		^"Empresa: ", empleados, " Votantes: ", votantes, " Propuestas: ", propuestas, " Presupuesto: ", presupuesto
```

**Main**

```smalltalk
Main
	empleado1 <- Empleado new initializeNombre: 'Juan' initializeSalario: 1000
	empleado2 <- Empleado new initializeNombre: 'María' initializeSalario: 1000
	empleado3 <- Empleado new initializeNombre: 'Pedro' initializeSalario: 1000

	votante1 <- Votante new initialize: empleado1
	votante2 <- Votante new initialize: empleado2
	votante3 <- Votante new initialize: empleado3

	propuesta1 <- Propuesta new initializeNombre: 'Propuesta 1' initializeSalarios: {empleado1 -> 1200, empleado2 -> 1200, empleado3 -> 1200}
	propuesta2 <- Propuesta new initializeNombre: 'Propuesta 2' initializeSalarios: {empleado1 -> 1400, empleado2 -> 1300, empleado3 -> 1100}

	empresa <- Empresa new initialize: {empleado1, empleado2, empleado3}
	empresa añadirVotante: votante1
	empresa añadirVotante: votante2
	empresa añadirVotante: votante3
	empresa añadirPropuesta: propuesta1
	empresa añadirPropuesta: propuesta2
	empresa setPresupuesto: 5000

	empresa votar: propuesta1
	empresa calcularSalarios
	empresa pagarSalarios
```

**Explicación del Código**

El código comienza definiendo la clase Empleado, que representa a los trabajadores de la empresa. La clase Empleado tiene dos variables de instancia: nombre y salario.

A continuación, se define la clase Votante, que representa a los trabajadores que pueden votar en la política democrática de salarios. La clase Votante tiene dos variables de instancia: empleado y votos.

La clase Propuesta representa las diferentes propuestas de distribución de salarios que se pueden votar. La clase Propuesta tiene dos variables de