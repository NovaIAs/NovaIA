```kotlin
// Esta clase modela una cuenta bancaria.
class CuentaBancaria(
    val nombre: String,
    val saldo: Double,
    val tipo: TipoCuenta
) {

    // Enum que representa los diferentes tipos de cuentas bancarias.
    enum class TipoCuenta {
        AHORROS,
        CORRIENTE,
        INVERSIÓN,
        FONDO_MONETARIO
    }

    // Función para depositar dinero en la cuenta.
    fun depositar(cantidad: Double) {
        if (cantidad <= 0) {
            throw IllegalArgumentException("La cantidad a depositar debe ser positiva.")
        }
        saldo += cantidad
    }

    // Función para retirar dinero de la cuenta.
    fun retirar(cantidad: Double): Boolean {
        if (cantidad <= 0) {
            throw IllegalArgumentException("La cantidad a retirar debe ser positiva.")
        }
        if (saldo < cantidad) {
            return false
        }
        saldo -= cantidad
        return true
    }

    // Función para transferir dinero a otra cuenta.
    fun transferir(cantidad: Double, cuentaDestino: CuentaBancaria) {
        if (cantidad <= 0) {
            throw IllegalArgumentException("La cantidad a transferir debe ser positiva.")
        }
        if (saldo < cantidad) {
            throw IllegalArgumentException("Saldo insuficiente para realizar la transferencia.")
        }
        saldo -= cantidad
        cuentaDestino.saldo += cantidad
    }

    // Función para obtener el saldo de la cuenta.
    fun getSaldo(): Double {
        return saldo
    }

    // Función para obtener el tipo de cuenta.
    fun getTipo(): TipoCuenta {
        return tipo
    }

    // Función para imprimir el estado de la cuenta.
    fun imprimirEstado() {
        println("Nombre: $nombre")
        println("Saldo: $saldo")
        println("Tipo de cuenta: ${tipo.name}")
    }
}

// Esta clase modela un banco.
class Banco {

    // Lista de cuentas bancarias del banco.
    private val cuentas = mutableListOf<CuentaBancaria>()

    // Función para crear una nueva cuenta bancaria.
    fun crearCuenta(nombre: String, saldo: Double, tipo: CuentaBancaria.TipoCuenta): CuentaBancaria {
        val cuenta = CuentaBancaria(nombre, saldo, tipo)
        cuentas.add(cuenta)
        return cuenta
    }

    // Función para obtener una cuenta bancaria por su número de cuenta.
    fun obtenerCuenta(numeroCuenta: Int): CuentaBancaria? {
        return cuentas.find { it.numeroCuenta == numeroCuenta }
    }

    // Función para imprimir el estado de todas las cuentas bancarias del banco.
    fun imprimirEstadoCuentas() {
        cuentas.forEach { it.imprimirEstado() }
    }
}

// Función principal del programa.
fun main() {
    // Creamos un banco.
    val banco = Banco()

    // Creamos algunas cuentas bancarias.
    val cuentaAhorros1 = banco.crearCuenta("Juan García", 1000.0, CuentaBancaria.TipoCuenta.AHORROS)
    val cuentaCorriente2 = banco.crearCuenta("María Pérez", 2000.0, CuentaBancaria.TipoCuenta.CORRIENTE)
    val cuentaInversion3 = banco.crearCuenta("Pedro López", 3000.0, CuentaBancaria.TipoCuenta.INVERSIÓN)

    // Realizamos algunas transacciones.
    cuentaAhorros1.depositar(500.0)
    cuentaCorriente2.retirar(1000.0)
    cuentaInversion3.transferir(1000.0, cuentaAhorros1)

    // Imprimimos el estado de todas las cuentas bancarias del banco.
    banco.imprimirEstadoCuentas()
}
```

Este código crea una clase `CuentaBancaria` que representa una cuenta bancaria con un nombre, un saldo y un tipo de cuenta. También incluye funciones para depositar, retirar y transferir dinero.

La clase `Banco` representa un banco que contiene una lista de cuentas bancarias. Incluye funciones para crear, obtener e imprimir el estado de las cuentas bancarias.

La función principal del programa crea un banco, crea algunas cuentas bancarias y realiza algunas transacciones. Finalmente, imprime el estado de todas las cuentas bancarias del banco.