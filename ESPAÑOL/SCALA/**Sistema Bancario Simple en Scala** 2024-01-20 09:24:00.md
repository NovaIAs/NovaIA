```scala
// Definición de la clase CuentaBancaria
class CuentaBancaria(val titular: String, val numeroCuenta: String, var saldo: Double) {

  // Método para depositar dinero
  def depositar(cantidad: Double): Unit = {
    saldo += cantidad
  }

  // Método para retirar dinero
  def retirar(cantidad: Double): Unit = {
    if (cantidad <= saldo) {
      saldo -= cantidad
    } else {
      // Si se intenta retirar más dinero del que hay en la cuenta, se muestra un mensaje de error
      println(s"No hay suficiente saldo en la cuenta. Saldo actual: $saldo")
    }
  }

  // Método para obtener el saldo de la cuenta
  def consultarSaldo(): Double = {
    saldo
  }

  // Método para imprimir los datos de la cuenta
  def imprimirDatos(): Unit = {
    println(s"Titular: $titular")
    println(s"Número de cuenta: $numeroCuenta")
    println(s"Saldo: $saldo")
  }
}

// Definición de la clase Banco
class Banco {

  // Lista de cuentas bancarias
  private var cuentasBancarias: List[CuentaBancaria] = List()

  // Método para crear una nueva cuenta bancaria
  def crearCuentaBancaria(titular: String, numeroCuenta: String, saldo: Double): Unit = {
    cuentasBancarias = cuentasBancarias :+ new CuentaBancaria(titular, numeroCuenta, saldo)
  }

  // Método para obtener una cuenta bancaria por su número
  def obtenerCuentaBancaria(numeroCuenta: String): CuentaBancaria = {
    cuentasBancarias.find(_.numeroCuenta == numeroCuenta).get
  }

  // Método para depositar dinero en una cuenta bancaria
  def depositarDinero(numeroCuenta: String, cantidad: Double): Unit = {
    val cuentaBancaria = obtenerCuentaBancaria(numeroCuenta)
    cuentaBancaria.depositar(cantidad)
  }

  // Método para retirar dinero de una cuenta bancaria
  def retirarDinero(numeroCuenta: String, cantidad: Double): Unit = {
    val cuentaBancaria = obtenerCuentaBancaria(numeroCuenta)
    cuentaBancaria.retirar(cantidad)
  }

  // Método para obtener el saldo de una cuenta bancaria
  def consultarSaldo(numeroCuenta: String): Double = {
    val cuentaBancaria = obtenerCuentaBancaria(numeroCuenta)
    cuentaBancaria.consultarSaldo()
  }

  // Método para imprimir los datos de todas las cuentas bancarias
  def imprimirDatosCuentasBancarias(): Unit = {
    cuentasBancarias.foreach(_.imprimirDatos())
  }
}

// Creación de un objeto de la clase Banco
val banco = new Banco()

// Creación de una cuenta bancaria
banco.crearCuentaBancaria("Juan Pérez", "123456789", 1000.0)

// Depósito de dinero en la cuenta bancaria
banco.depositarDinero("123456789", 500.0)

// Retiro de dinero de la cuenta bancaria
banco.retirarDinero("123456789", 300.0)

// Consulta del saldo de la cuenta bancaria
val saldo = banco.consultarSaldo("123456789")

// Impresión de los datos de la cuenta bancaria
banco.imprimirDatosCuentasBancarias()

// Asignación de un nuevo valor al saldo de la cuenta bancaria
banco.obtenerCuentaBancaria("123456789").saldo = 2000.0

// Impresión del nuevo saldo de la cuenta bancaria
println(s"Nuevo saldo: ${banco.obtenerCuentaBancaria("123456789").saldo}")
```

Este código crea un sistema bancario simple, con una clase `CuentaBancaria` que representa una cuenta bancaria y una clase `Banco` que representa un banco con una colección de cuentas bancarias.

El código permite crear cuentas bancarias, depositar y retirar dinero de las cuentas, consultar el saldo de las cuentas e imprimir los datos de todas las cuentas bancarias.

Además, el código permite modificar el saldo de una cuenta bancaria directamente, lo que es una mala práctica en un sistema bancario real, ya que puede permitir a los usuarios modificar su saldo sin autorización.

Este código es un **ejemplo avanzado** de Scala que muestra cómo crear clases, objetos, listas y métodos, y cómo interactuar con los datos de los objetos. El código también muestra cómo usar la sintaxis de Scala para definir funciones, bucles y expresiones condicionales.