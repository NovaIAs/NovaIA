```groovy
// Clase para representar una cuenta bancaria
class CuentaBancaria {

    // Propiedades de la cuenta bancaria
    private String nombreTitular
    private BigDecimal saldo

    // Constructor de la clase
    public CuentaBancaria(String nombreTitular, BigDecimal saldo) {
        this.nombreTitular = nombreTitular
        this.saldo = saldo
    }

    // Método para depositar dinero en la cuenta
    public void depositar(BigDecimal monto) {
        saldo += monto
    }

    // Método para retirar dinero de la cuenta
    public void retirar(BigDecimal monto) {
        if (monto <= saldo) {
            saldo -= monto
        } else {
            throw new IllegalArgumentException("No hay suficiente saldo en la cuenta")
        }
    }

    // Método para obtener el saldo de la cuenta
    public BigDecimal getSaldo() {
        return saldo
    }

    // Método para obtener el nombre del titular de la cuenta
    public String getNombreTitular() {
        return nombreTitular
    }

    // Método para obtener una representación en cadena de la cuenta bancaria
    @Override
    public String toString() {
        return "CuentaBancaria{" +
                "nombreTitular='" + nombreTitular + '\'' +
                ", saldo=" + saldo +
                '}'
    }
}

// Clase para representar un banco
class Banco {

    // Lista de las cuentas bancarias del banco
    private List<CuentaBancaria> cuentasBancarias

    // Constructor de la clase
    public Banco() {
        this.cuentasBancarias = []
    }

    // Método para agregar una cuenta bancaria al banco
    public void agregarCuentaBancaria(CuentaBancaria cuentaBancaria) {
        cuentasBancarias.add(cuentaBancaria)
    }

    // Método para obtener una lista de todas las cuentas bancarias del banco
    public List<CuentaBancaria> getCuentasBancarias() {
        return cuentasBancarias
    }

    // Método para obtener el saldo total de todas las cuentas bancarias del banco
    public BigDecimal getSaldoTotal() {
        BigDecimal saldoTotal = BigDecimal.ZERO
        for (CuentaBancaria cuentaBancaria : cuentasBancarias) {
            saldoTotal += cuentaBancaria.getSaldo()
        }
        return saldoTotal
    }

    // Método para obtener una representación en cadena del banco
    @Override
    public String toString() {
        return "Banco{" +
                "cuentasBancarias=" + cuentasBancarias +
                '}'
    }
}

// Clase principal del programa
class Main {

    public static void main(String[] args) {
        // Creamos un banco
        Banco banco = new Banco()

        // Creamos tres cuentas bancarias y las agregamos al banco
        CuentaBancaria cuenta1 = new CuentaBancaria("Juan Pérez", new BigDecimal("1000"))
        CuentaBancaria cuenta2 = new CuentaBancaria("María Martínez", new BigDecimal("2000"))
        CuentaBancaria cuenta3 = new CuentaBancaria("Pedro López", new BigDecimal("3000"))
        banco.agregarCuentaBancaria(cuenta1)
        banco.agregarCuentaBancaria(cuenta2)
        banco.agregarCuentaBancaria(cuenta3)

        // Imprimimos el saldo total del banco
        println("Saldo total del banco: ${banco.getSaldoTotal()}")

        // Imprimimos las cuentas bancarias del banco
        println("Cuentas bancarias del banco:")
        for (CuentaBancaria cuentaBancaria : banco.getCuentasBancarias()) {
            println(cuentaBancaria)
        }
    }
}
```

Explicación del código:

* La clase `CuentaBancaria` representa una cuenta bancaria. Tiene las propiedades `nombreTitular` y `saldo`, y los métodos `depositar()`, `retirar()`, `getSaldo()` y `getNombreTitular()`.
* La clase `Banco` representa un banco. Tiene la propiedad `cuentasBancarias`, que es una lista de cuentas bancarias. También tiene los métodos `agregarCuentaBancaria()`, `getCuentasBancarias()`, `getSaldoTotal()` y `toString()`.
* La clase `Main` es la clase principal del programa. Crea un banco, añade tres cuentas bancarias al banco, imprime el saldo total del banco e imprime las cuentas bancarias del banco.