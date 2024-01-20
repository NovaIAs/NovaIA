```smalltalk
**Clase Persona**

Persona clase |nombre|apellido|edad|

Persona nuevo |nombre|apellido|edad|
	^Persona nuevo conNombre: nombre conApellido: apellido conEdad: edad

Persona conNombre: |nombre|
	^self clase nuevo conNombre: nombre conApellido: '' conEdad: 0

Persona conApellido: |apellido|
	^self clase nuevo conNombre: '' conApellido: apellido conEdad: 0

Persona conEdad: |edad|
	^self clase nuevo conNombre: '' conApellido: '' conEdad: edad

Persona nombre
	^nombre

Persona apellido
	^apellido

Persona edad
	^edad

Persona nombre: |nombre|
	nombre := nombre.

Persona apellido: |apellido|
	apellido := apellido.

Persona edad: |edad|
	edad := edad.

Persona imprimir
	'Persona {nombre: ' , nombre , ', apellido: ' , apellido ,
		', edad: ' , edad , '}' imprimirNl

**Clase Empleado**

Empleado subclase de: Persona

Empleado nuevo |nombre|apellido|edad|sueldo|
	^Empleado nuevo conNombre: nombre conApellido: apellido conEdad: edad conSueldo: sueldo

Empleado conSueldo: |sueldo|
	^self clase nuevo conNombre: '' conApellido: '' conEdad: 0 conSueldo: sueldo

Empleado sueldo
	^sueldo

Empleado sueldo: |sueldo|
	sueldo := sueldo.

Empleado imprimir
	'Empleado {nombre: ' , nombre , ', apellido: ' , apellido , ', edad: ' , edad ,
    	', sueldo: ' , sueldo , '}' imprimirNl

**Clase Principal**

Principal clase

Principal iniciar
	|juan empleado|
	juan := Persona nuevo conNombre: 'Juan' conApellido: 'Pérez' conEdad: 25.
	empleado := Empleado nuevo conNombre: 'María' conApellido: 'Gómez' conEdad: 30 conSueldo: 1000.
	juan imprimir.
	empleado imprimir.

```

**Explicación:**

1. **Clase Persona:**
   * Es una clase que representa a una persona con atributos como nombre, apellido y edad.
   * Proporciona métodos para inicializar, obtener y modificar estos atributos.
   * También proporciona un método `imprimir` para imprimir la información de la persona.

2. **Clase Empleado:**
   * Es una subclase de `Persona` que representa específicamente a un empleado.
   * Añade un nuevo atributo `sueldo` y proporciona métodos para obtener y modificar este atributo.
   * También proporciona un método `imprimir` para imprimir la información del empleado.

3. **Clase Principal:**
   * Es la clase principal que contiene el método `iniciar` que se ejecuta al iniciar la aplicación.
   * En este método, se crean instancias de las clases `Persona` y `Empleado` y se llama a sus métodos `imprimir` para mostrar la información de las personas y los empleados.

Al ejecutar el programa, se crearán instancias de las clases `Persona` y `Empleado`, y se imprimirá la información de estas instancias en la consola.