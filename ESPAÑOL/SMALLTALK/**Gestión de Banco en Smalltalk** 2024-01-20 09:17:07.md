```smalltalk
```

**1. Clase `Persona`:**

```smalltalk
Clase Persona
    variables de instancia
        nombre
        edad
    métodos
        initialize: unNombre unaEdad
            "Crea una nueva persona"
            nombre := unNombre
            edad := unaEdad
        nombre
            "Devuelve el nombre de la persona"
            ^nombre
        edad
            "Devuelve la edad de la persona"
            ^edad
```

**2. Clase `Empleado`:**

```smalltalk
Clase Empleado superclase: Persona
    variables de instancia
        salario
    métodos
        initialize: unNombre unaEdad unSalario
            "Crea un nuevo empleado"
            super initialize: unNombre unaEdad
            salario := unSalario
        salario
            "Devuelve el salario del empleado"
            ^salario
```

**3. Clase `Cliente`:**

```smalltalk
Clase Cliente superclase: Persona
    variables de instancia
        número de cliente
    métodos
        initialize: unNombre unaEdad unNúmero de cliente
            "Crea un nuevo cliente"
            super initialize: unNombre unaEdad
            número de cliente := unNúmero de cliente
        número de cliente
            "Devuelve el número de cliente"
            ^número de cliente
```

**4. Clase `CuentaBancaria`:**

```smalltalk
Clase CuentaBancaria
    variables de instancia
        titular
        saldo
    métodos
        initialize: unTitular unSaldo
            "Crea una nueva cuenta bancaria"
            titular := unTitular
            saldo := unSaldo
        titular
            "Devuelve el titular de la cuenta"
            ^titular
        saldo
            "Devuelve el saldo de la cuenta"
            ^saldo
        depositar: unaCantidad
            "Deposita una cantidad de dinero en la cuenta"
            saldo := saldo + unaCantidad
        retirar: unaCantidad
            "Retira una cantidad de dinero de la cuenta"
            saldo := saldo - unaCantidad
```

**5. Clase `Banco`:**

```smalltalk
Clase Banco
    variables de instancia
        clientes
        cuentas
    métodos
        initialize
            "Crea un nuevo banco"
            clientes := OrderedCollection new
            cuentas := OrderedCollection new
        añadirCliente: unCliente
            "Añade un cliente al banco"
            clientes add: unCliente
        añadirCuenta: unaCuenta
            "Añade una cuenta al banco"
            cuentas add: unaCuenta
        obtenerCliente: unNombre
            "Obtiene un cliente del banco"
            ^clientes detect: [:cliente | cliente nombre = unNombre]
        obtenerCuenta: unNúmero de cuenta
            "Obtiene una cuenta del banco"
            ^cuentas detect: [:cuenta | cuenta número de cuenta = unNúmero de cuenta]
```

**6. Ejemplo de uso:**

```smalltalk
"Crea un nuevo banco"
banco := Banco new

"Añade un cliente al banco"
banco añadirCliente: Persona new nombre: 'Juan' edad: 30

"Añade una cuenta al banco"
cuenta := CuentaBancaria new titular: 'Juan' saldo: 1000

"Añade la cuenta al banco"
banco añadirCuenta: cuenta

"Obtiene el cliente del banco"
cliente := banco obtenerCliente: 'Juan'

"Obtiene la cuenta del cliente"
cuenta := banco obtenerCuenta: cliente número de cliente

"Deposita dinero en la cuenta"
cuenta depositar: 500

"Retira dinero de la cuenta"
cuenta retirar: 200

"Muestra el saldo de la cuenta"
Transcript show: cuenta saldo
```

Este código crea un banco, añade un cliente al banco, añade una cuenta al banco, obtiene el cliente del banco, obtiene la cuenta del cliente, deposita dinero en la cuenta, retira dinero de la cuenta y muestra el saldo de la cuenta.