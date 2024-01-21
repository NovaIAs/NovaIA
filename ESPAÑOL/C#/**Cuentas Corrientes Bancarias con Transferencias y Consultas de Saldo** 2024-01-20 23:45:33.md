```c#
// Declaración de una clase "CuentaCorriente" para representar una cuenta corriente en un banco.
public class CuentaCorriente
{
    // Atributos privados de la cuenta corriente (encapsulación).
    private decimal saldo;
    private string titular;
    private string numeroCuenta;

    // Constructor de la cuenta corriente, inicializa los atributos con los valores proporcionados.
    public CuentaCorriente(string titular, string numeroCuenta, decimal saldoInicial)
    {
        this.titular = titular;
        this.numeroCuenta = numeroCuenta;
        this.saldo = saldoInicial;
    }

    // Métodos de la cuenta corriente para realizar operaciones bancarias.

    // Método para depositar dinero en la cuenta.
    public void Depositar(decimal cantidad)
    {
        if (cantidad > 0)
            this.saldo += cantidad;
    }

    // Método para retirar dinero de la cuenta (si hay suficiente saldo).
    public bool Retirar(decimal cantidad)
    {
        if (cantidad > 0 && cantidad <= this.saldo)
        {
            this.saldo -= cantidad;
            return true;
        }
        else
        {
            return false; // No se pudo retirar la cantidad solicitada.
        }
    }

    // Método para consultar el saldo de la cuenta.
    public decimal ConsultarSaldo()
    {
        return this.saldo;
    }

    // Método para transferir dinero a otra cuenta corriente.
    public bool Transferir(CuentaCorriente cuentaDestino, decimal cantidad)
    {
        if (cantidad > 0 && cantidad <= this.saldo)
        {
            this.saldo -= cantidad;
            cuentaDestino.Depositar(cantidad);
            return true;
        }
        else
        {
            return false; // No se pudo realizar la transferencia.
        }
    }

    // Método para obtener una representación textual de la cuenta corriente.
    public override string ToString()
    {
        return $"Cuenta {this.numeroCuenta} a nombre de {this.titular} con saldo {this.saldo:C2}";
    }
}

// Clase principal "Banco" para gestionar un conjunto de cuentas corrientes.
public class Banco
{
    // Lista para almacenar las cuentas corrientes del banco.
    private List<CuentaCorriente> cuentasCorrientes;

    // Constructor del banco, inicializa la lista de cuentas.
    public Banco()
    {
        this.cuentasCorrientes = new List<CuentaCorriente>();
    }

    // Método para agregar una nueva cuenta corriente al banco.
    public void AgregarCuenta(CuentaCorriente cuenta)
    {
        this.cuentasCorrientes.Add(cuenta);
    }

    // Método para buscar una cuenta corriente por su número de cuenta.
    public CuentaCorriente BuscarCuenta(string numeroCuenta)
    {
        foreach (var cuenta in this.cuentasCorrientes)
        {
            if (cuenta.numeroCuenta == numeroCuenta)
                return cuenta;
        }
        return null; // No se encontró la cuenta.
    }

    // Método para transferir dinero entre dos cuentas corrientes.
    public bool Transferir(string numeroCuentaOrigen, string numeroCuentaDestino, decimal cantidad)
    {
        // Buscar las cuentas de origen y destino.
        CuentaCorriente cuentaOrigen = this.BuscarCuenta(numeroCuentaOrigen);
        CuentaCorriente cuentaDestino = this.BuscarCuenta(numeroCuentaDestino);

        // Si las cuentas existen y se puede realizar la transferencia, realizarla.
        if (cuentaOrigen != null && cuentaDestino != null)
        {
            return cuentaOrigen.Transferir(cuentaDestino, cantidad);
        }
        else
        {
            return false; // No se pudo realizar la transferencia.
        }
    }

    // Método para obtener una representación textual del banco.
    public override string ToString()
    {
        StringBuilder sb = new StringBuilder();
        sb.AppendLine("Estado del banco:");
        foreach (var cuenta in this.cuentasCorrientes)
        {
            sb.AppendLine(cuenta.ToString());
        }
        return sb.ToString();
    }
}

// Clase principal para probar el código.
public class Program
{
    public static void Main()
    {
        // Crear un banco.
        Banco banco = new Banco();

        // Crear algunas cuentas corrientes y agregarlas al banco.
        banco.AgregarCuenta(new CuentaCorriente("Juan Pérez", "ES1234567890", 1000));
        banco.AgregarCuenta(new CuentaCorriente("María Gómez", "ES9876543210", 2000));
        banco.AgregarCuenta(new CuentaCorriente("Pedro Sánchez", "ES1122334455", 3000));

        // Realizar una transferencia entre dos cuentas.
        banco.Transferir("ES1234567890", "ES9876543210", 500);

        // Consultar el saldo de una cuenta.
        decimal saldoCuentaJuan = banco.BuscarCuenta("ES1234567890").ConsultarSaldo();

        // Mostrar el estado del banco.
        Console.WriteLine(banco);

        // Mostrar el saldo de la cuenta de Juan Pérez.
        Console.WriteLine($"Saldo de la cuenta de Juan Pérez: {saldoCuentaJuan:C2}");
    }
}
```

Este código crea una clase "CuentaCorriente" para representar una cuenta corriente en un banco. La clase tiene atributos privados para el saldo, nombre del titular y número de cuenta, y métodos para realizar operaciones bancarias como depositar, retirar, transferir y consultar el saldo.

También se crea una clase "Banco" para gestionar un conjunto de cuentas corrientes. El banco tiene una lista de cuentas corrientes y métodos para agregar nuevas cuentas, buscar cuentas por su número de cuenta y transferir dinero entre cuentas.

En la clase "Program" se crea un banco y se agregan algunas cuentas corrientes. Luego se realiza una transferencia entre dos cuentas y se consulta el saldo de una cuenta. Finalmente, se muestra el estado del banco y el saldo de la cuenta de Juan Pérez.

Este código es bastante complejo y abarca varios conceptos de programación en C#, como encapsulación, herencia, polimorfismo, listas y métodos. También demuestra cómo se pueden crear y utilizar clases y objetos para representar entidades del mundo real y realizar operaciones sobre ellas.