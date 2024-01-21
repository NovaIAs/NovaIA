```kotlin
/**
 * Clase que representa una cuenta bancaria.
 */
class CuentaBancaria(
    /**
     * El nombre del titular de la cuenta.
     */
    val titular: String,

    /**
     * El número de cuenta.
     */
    val numero: String,

    /**
     * El saldo de la cuenta.
     */
    var saldo: Double = 0.0
) {

    /**
     * Método para ingresar dinero en la cuenta.
     */
    fun ingresar(cantidad: Double) {
        if (cantidad > 0) {
            saldo += cantidad
        }
    }

    /**
     * Método para retirar dinero de la cuenta.
     */
    fun retirar(cantidad: Double): Boolean {
        if (cantidad > 0 && cantidad <= saldo) {
            saldo -= cantidad
            return true
        }
        return false
    }

    /**
     * Método para transferir dinero de una cuenta a otra.
     */
    fun transferir(cantidad: Double, cuentaDestino: CuentaBancaria): Boolean {
        if (cantidad > 0 && cantidad <= saldo) {
            saldo -= cantidad
            cuentaDestino.saldo += cantidad
            return true
        }
        return false
    }
}

/**
 * Clase que representa un cajero automático.
 */
class CajeroAutomatico {

    /**
     * Método para iniciar una sesión en el cajero automático.
     */
    fun iniciarSesion(tarjeta: TarjetaBancaria, pin: String): Boolean {
        if (tarjeta.pin == pin) {
            return true
        }
        return false
    }

    /**
     * Método para consultar el saldo de una cuenta.
     */
    fun consultarSaldo(cuenta: CuentaBancaria): Double {
        return cuenta.saldo
    }

    /**
     * Método para ingresar dinero en una cuenta.
     */
    fun ingresarDinero(cuenta: CuentaBancaria, cantidad: Double) {
        cuenta.ingresar(cantidad)
    }

    /**
     * Método para retirar dinero de una cuenta.
     */
    fun retirarDinero(cuenta: CuentaBancaria, cantidad: Double): Boolean {
        return cuenta.retirar(cantidad)
    }

    /**
     * Método para transferir dinero de una cuenta a otra.
     */
    fun transferirDinero(cuentaOrigen: CuentaBancaria, cuentaDestino: CuentaBancaria, cantidad: Double): Boolean {
        return cuentaOrigen.transferir(cantidad, cuentaDestino)
    }
}

/**
 * Clase que representa una tarjeta bancaria.
 */
class TarjetaBancaria(

    /**
     * El número de la tarjeta.
     */
    val numero: String,

    /**
     * El PIN de la tarjeta.
     */
    val pin: String
)

/**
 * Función principal del programa.
 */
fun main(args: Array<String>) {

    // Crear una cuenta bancaria
    val cuenta = CuentaBancaria("Juan Pérez", "ES12345678901234567890", 1000.0)

    // Crear una tarjeta bancaria
    val tarjeta = TarjetaBancaria("1234567890123456", "1234")

    // Crear un cajero automático
    val cajero = CajeroAutomatico()

    // Iniciar sesión en el cajero automático
    if (cajero.iniciarSesion(tarjeta, "1234")) {

        // Consultar el saldo de la cuenta
        val saldo = cajero.consultarSaldo(cuenta)

        // Ingresar dinero en la cuenta
        cajero.ingresarDinero(cuenta, 500.0)

        // Retirar dinero de la cuenta
        cajero.retirarDinero(cuenta, 300.0)

        // Transferir dinero a otra cuenta
        cajero.transferirDinero(cuenta, CuentaBancaria("María García", "ES98765432109876543210"), 200.0)

        // Mostrar el saldo final de la cuenta
        println("Saldo final: ${saldo}")

    } else {

        // Mostrar un mensaje de error
        println("Error al iniciar sesión")

    }
}
```