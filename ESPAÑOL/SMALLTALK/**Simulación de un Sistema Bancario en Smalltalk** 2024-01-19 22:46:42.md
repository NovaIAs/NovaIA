```smalltalk
# Clase que representa un objeto Persona
Persona clase {
   nombre: 'Juan';
   apellido: 'Pérez';
   edad: 30;

   inicializar: unNombre unApellido unaEdad {
      nombre := unNombre;
      apellido := unApellido;
      edad := unaEdad;
   }

   nombreCompleto {
      nombre + ' ' + apellido
   }

   mayorEdad {
      edad > 18
   }
}

# Clase que representa un objeto CuentaBancaria
CuentaBancaria clase {
   numeroCuenta: '123456789';
   saldo: 1000;

   inicializar: unNumeroCuenta unSaldo {
      numeroCuenta := unNumeroCuenta;
      saldo := unSaldo;
   }

   depositar: unaCantidad {
      saldo := saldo + unaCantidad;
   }

   retirar: unaCantidad {
      if (unaCantidad <= saldo) {[
         saldo := saldo - unaCantidad;
         true
      ]} else {[
         false
      ]};
   }

   consultarSaldo {
      saldo
   }
}

# Clase que representa un objeto SistemaBancario
SistemaBancario clase {
   clientes: Colección nueva;

   agregarCliente: unCliente {
      clientes añadir: unCliente;
   }

   buscarCliente: unNombre unApellido {
      clientes seleccionar: [ :cliente | cliente nombreCompleto = (unNombre + ' ' + unApellido) ];
   }

   crearCuentaBancaria: unCliente unNumeroCuenta unSaldo {
      CuentaBancaria nueva inicializar: unNumeroCuenta unSaldo;
      unCliente agregarCuentaBancaria: cuentaNueva;
   }
}

# Crear un sistema bancario
sistemaBancario := SistemaBancario nueva;

# Crear algunos clientes
cliente1 := Persona nueva inicializar: 'Juan' 'Pérez' 30;
cliente2 := Persona nueva inicializar: 'María' 'López' 25;

# Agregar los clientes al sistema bancario
sistemaBancario agregarCliente: cliente1;
sistemaBancario agregarCliente: cliente2;

# Crear una cuenta bancaria para cada cliente
sistemaBancario crearCuentaBancaria: cliente1 '123456789' 1000;
sistemaBancario crearCuentaBancaria: cliente2 '987654321' 500;

# Depositar dinero en las cuentas bancarias
cliente1 cuentaBancaria depositar: 500;
cliente2 cuentaBancaria depositar: 250;

# Retirar dinero de las cuentas bancarias
cliente1 cuentaBancaria retirar: 300;
cliente2 cuentaBancaria retirar: 100;

# Consultar el saldo de las cuentas bancarias
cliente1 cuentaBancaria consultarSaldo;
cliente2 cuentaBancaria consultarSaldo;
```

Explicación del código:

1. Se definen las clases `Persona`, `CuentaBancaria` y `SistemaBancario`.

2. La clase `Persona` representa un objeto persona con atributos como nombre, apellido y edad.

3. La clase `CuentaBancaria` representa un objeto cuenta bancaria con atributos como número de cuenta y saldo.

4. La clase `SistemaBancario` representa un sistema bancario que gestiona los clientes y sus cuentas bancarias.

5. Se crean objetos de las clases `Persona` y `CuentaBancaria`.

6. Se agrega los clientes al sistema bancario.

7. Se crean cuentas bancarias para cada cliente.

8. Se deposita dinero en las cuentas bancarias.

9. Se retira dinero de las cuentas bancarias.

10. Se consulta el saldo de las cuentas bancarias.

11. Se muestran los resultados en la consola.