```smalltalk
**Clase Persona:**

```smalltalk
Persona clase

nombre: en 'nombre'
edad: en 'edad'

inicializar: unNombre conEdad: unaEdad
    super inicializar.
    nombre := unNombre.
    edad := unaEdad.

edad: en 'edad'
    ^edad

nombre: en 'nombre'
    ^nombre
```

**Clase Empleado:**

```smalltalk
Empleado clase
hereda: de Persona

salario: en 'salario'

inicializar: unNombre conEdad: unaEdad conSalario: unSalario
    super inicializar: unNombre conEdad: unaEdad.
    salario := unSalario.

salario: en 'salario'
    ^salario
```

**Clase Cliente:**

```smalltalk
Cliente clase
hereda: de Persona

dirección: en 'dirección'

inicializar: unNombre conEdad: unaEdad conDirección: unaDirección
    super inicializar: unNombre conEdad: unaEdad.
    dirección := unaDirección.

dirección: en 'dirección'
    ^dirección
```

**Clase Cuenta:**

```smalltalk
Cuenta clase

saldo: en 'saldo'

inicializar: unSaldo
    super inicializar.
    saldo := unSaldo.

saldo: en 'saldo'
    ^saldo

depositar: unMonto
    saldo := saldo + unMonto.

retirar: unMonto
    saldo := saldo - unMonto.
```

**Ejemplo de uso:**

```smalltalk
unEmpleado := Empleado nuevo
    inicializar: 'Juan García'
    conEdad: 30
    conSalario: 1000.

unCliente := Cliente nuevo
    inicializar: 'María González'
    conEdad: 25
    conDirección: 'Calle 123'.

unaCuenta := Cuenta nueva
    inicializar: 500.

unEmpleado depositar: 200 en: unaCuenta.
unCliente retirar: 100 de: unaCuenta.

unEmpleado edad.
unaCuenta saldo.
```

Explicación del código:

* Se definen tres clases: **Persona**, **Empleado** y **Cliente**. La clase **Persona** es la clase base de las otras dos clases.
* Las clases **Empleado** y **Cliente** heredan de la clase **Persona**, lo que significa que tienen todas las variables de instancia y métodos de la clase **Persona**, además de sus propias variables de instancia y métodos.
* La clase **Cuenta** no hereda de ninguna otra clase.
* Se definen métodos en las clases para inicializar objetos, obtener y establecer valores de variables de instancia, y realizar operaciones en los objetos.
* Se crea un objeto de la clase **Empleado** llamado **unEmpleado**, un objeto de la clase **Cliente** llamado **unCliente** y un objeto de la clase **Cuenta** llamado **unaCuenta**.
* Se llama a los métodos de los objetos para depositar y retirar dinero de la cuenta.
* Se llama a los métodos de los objetos para obtener la edad de un empleado, el saldo de una cuenta, etc.

Este código es solo un ejemplo de cómo se pueden usar las clases y objetos en Smalltalk. Hay muchas otras formas de usar las clases y objetos en Smalltalk, y se pueden usar para resolver una amplia variedad de problemas.