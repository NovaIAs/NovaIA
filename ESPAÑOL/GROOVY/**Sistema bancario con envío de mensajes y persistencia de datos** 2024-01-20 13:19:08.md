```groovy
// Definimos una clase para representar una cuenta bancaria.
class CuentaBancaria {
    // Atributos privados para almacenar el número de cuenta y el saldo.
    private String numeroCuenta
    private BigDecimal saldo

    // Constructor para inicializar la cuenta con un número de cuenta y un saldo inicial.
    CuentaBancaria(String numeroCuenta, BigDecimal saldoInicial) {
        this.numeroCuenta = numeroCuenta
        this.saldo = saldoInicial
    }

    // Método para depositar dinero en la cuenta.
    void depositar(BigDecimal monto) {
        saldo += monto
    }

    // Método para retirar dinero de la cuenta.
    void retirar(BigDecimal monto) {
        if (monto <= saldo) {
            saldo -= monto
        } else {
            throw new IllegalArgumentException("No se puede retirar más dinero del que hay en la cuenta.")
        }
    }

    // Método para obtener el saldo de la cuenta.
    BigDecimal getSaldo() {
        return saldo
    }
}

// Creamos una instancia de la clase CuentaBancaria para representar la cuenta de Juan.
CuentaBancaria cuentaJuan = new CuentaBancaria("1234567890", new BigDecimal("1000.00"))

// Depositamos 500 pesos en la cuenta de Juan.
cuentaJuan.depositar(new BigDecimal("500.00"))

// Retiramos 300 pesos de la cuenta de Juan.
cuentaJuan.retirar(new BigDecimal("300.00"))

// Imprimimos el saldo final de la cuenta de Juan.
println("El saldo final de la cuenta de Juan es: ${cuentaJuan.getSaldo()}")


// Definimos una interfaz para representar un servicio de envío de mensajes.
interface ServicioMensajes {
    // Método para enviar un mensaje de texto.
    void enviarMensaje(String numeroTelefono, String mensaje)
}

// Creamos una clase para representar el servicio de envío de mensajes SMS.
class ServicioMensajesSMS implements ServicioMensajes {
    // Método para enviar un mensaje de texto.
    void enviarMensaje(String numeroTelefono, String mensaje) {
        println("Enviando mensaje de texto a: ${numeroTelefono}")
        println("Mensaje: ${mensaje}")
    }
}

// Creamos una clase para representar el servicio de envío de mensajes por correo electrónico.
class ServicioMensajesCorreoElectronico implements ServicioMensajes {
    // Método para enviar un mensaje de texto.
    void enviarMensaje(String numeroTelefono, String mensaje) {
        println("Enviando mensaje de correo electrónico a: ${numeroTelefono}")
        println("Mensaje: ${mensaje}")
    }
}

// Creamos una clase para representar un cliente.
class Cliente {
    // Atributos privados para almacenar el nombre, el número de teléfono y el servicio de mensajes preferido.
    private String nombre
    private String numeroTelefono
    private ServicioMensajes servicioMensajes

    // Constructor para inicializar el cliente con un nombre, un número de teléfono y un servicio de mensajes preferido.
    Cliente(String nombre, String numeroTelefono, ServicioMensajes servicioMensajes) {
        this.nombre = nombre
        this.numeroTelefono = numeroTelefono
        this.servicioMensajes = servicioMensajes
    }

    // Método para enviar un mensaje al cliente.
    void enviarMensaje(String mensaje) {
        servicioMensajes.enviarMensaje(numeroTelefono, mensaje)
    }
}

// Creamos una instancia de la clase Cliente para representar a Juan.
Cliente juan = new Cliente("Juan", "1234567890", new ServicioMensajesSMS())

// Enviamos un mensaje a Juan.
juan.enviarMensaje("Hola Juan, tu saldo actual es de ${cuentaJuan.getSaldo()}")


// Definimos una interfaz para representar un servicio de persistencia.
interface ServicioPersistencia {
    // Método para guardar un objeto en la base de datos.
    void guardar(Object objeto)

    // Método para cargar un objeto de la base de datos.
    Object cargar(Class clase, String id)
}

// Creamos una clase para representar el servicio de persistencia en memoria.
class ServicioPersistenciaMemoria implements ServicioPersistencia {
    // Mapa para almacenar objetos en memoria.
    private Map objetos = [:]

    // Método para guardar un objeto en la base de datos.
    void guardar(Object objeto) {
        objetos[objeto.id] = objeto
    }

    // Método para cargar un objeto de la base de datos.
    Object cargar(Class clase, String id) {
        objetos[id]
    }
}

// Creamos una clase para representar el servicio de persistencia en disco.
class ServicioPersistenciaDisco implements ServicioPersistencia {
    // Ruta del archivo donde se almacenarán los objetos.
    private String rutaArchivo

    // Constructor para inicializar el servicio de persistencia con la ruta del archivo donde se almacenarán los objetos.
    ServicioPersistenciaDisco(String rutaArchivo) {
        this.rutaArchivo = rutaArchivo