```cool
clase Principal {
    -- (1) Declaración de atributos y métodos.
    atributos:
        cuenta: String;
        saldo: Float;
        historial: Lista<Transacción>;

    métodos:
        depositar(cantidad: Float): Void {
            -- (2) Sumar cantidad al saldo.
            saldo := saldo + cantidad;
            -- (3) Añadir una transacción de depósito al historial.
            historial.añadir(new Transacción(cantidad, TipoTransacción.DEPÓSITO));
        }

        retirar(cantidad: Float): Void {
            -- (4) Comprobar si la cantidad es mayor que el saldo.
            if cantidad > saldo then
                -- (5) Lanzar una excepción.
                raise Error("Saldo insuficiente");
            end if;

            -- (6) Restar cantidad al saldo.
            saldo := saldo - cantidad;
            -- (7) Añadir una transacción de retiro al historial.
            historial.añadir(new Transacción(cantidad, TipoTransacción.RETIRO));
        }

        consultarSaldo(): Float {
            -- (8) Devolver el saldo.
            return saldo;
        }

        verHistorial(): Lista<Transacción> {
            -- (9) Devolver el historial de transacciones.
            return historial;
        }
}

clase Transacción {
    -- Atributos de la transacción.
    atributos:
        cantidad: Float;
        tipo: TipoTransacción;

    -- Constructor de la transacción.
    constructor(cantidad: Float, tipo: TipoTransacción) {
        this.cantidad = cantidad;
        this.tipo = tipo;
    }
}

enum TipoTransacción {
    DEPÓSITO,
    RETIRO
}

-- (10) Crear una instancia de la clase Principal.
cuenta = new Principal();

-- (11) Depositar 100€ en la cuenta.
cuenta.depositar(100.0);

-- (12) Retirar 50€ de la cuenta.
cuenta.retirar(50.0);

-- (13) Consultar el saldo de la cuenta.
saldo = cuenta.consultarSaldo();

-- (14) Ver el historial de transacciones de la cuenta.
historial = cuenta.verHistorial();

-- (15) Imprimir el saldo y el historial de transacciones.
print("Saldo: ", saldo);
print("Historial: ", historial);
```

Explicación del código:

* (1) Declaración de atributos y métodos: Se declaran los atributos y métodos de la clase Principal.
* (2) Sumar cantidad al saldo: Se suma la cantidad depositada al saldo.
* (3) Añadir una transacción de depósito al historial: Se añade una nueva transacción de depósito al historial de transacciones.
* (4) Comprobar si la cantidad es mayor que el saldo: Se comprueba si la cantidad a retirar es mayor que el saldo disponible.
* (5) Lanzar una excepción: Si la cantidad a retirar es mayor que el saldo, se lanza una excepción.
* (6) Restar cantidad al saldo: Se resta la cantidad retirada del saldo.
* (7) Añadir una transacción de retiro al historial: Se añade una nueva transacción de retiro al historial de transacciones.
* (8) Devolver el saldo: Se devuelve el saldo actual de la cuenta.
* (9) Devolver el historial de transacciones: Se devuelve el historial de transacciones de la cuenta.
* (10) Crear una instancia de la clase Principal: Se crea una nueva instancia de la clase Principal.
* (11) Depositar 100€ en la cuenta: Se depositan 100€ en la cuenta.
* (12) Retirar 50€ de la cuenta: Se retiran 50€ de la cuenta.
* (13) Consultar el saldo de la cuenta: Se consulta el saldo actual de la cuenta.
* (14) Ver el historial de transacciones de la cuenta: Se obtiene el historial de transacciones de la cuenta.
* (15) Imprimir el saldo y el historial de transacciones: Se imprimen el saldo y el historial de transacciones de la cuenta.