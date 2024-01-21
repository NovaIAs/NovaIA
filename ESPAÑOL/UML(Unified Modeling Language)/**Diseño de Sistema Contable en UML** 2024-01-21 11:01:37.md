```
Clase SistemaContable {
+ atributos:
  - cuentaCliente: Cuenta
  - cuentaBanco: Cuenta
  - cuentaVentas: Cuenta
  - cuentaGastos: Cuenta
  - cuentaBeneficios: Cuenta
+ métodos:
  - registrarVenta(importe: float): void
  - registrarGasto(importe: float): void
  - calcularBeneficios(): float
}

Clase Cuenta {
+ atributos:
  - saldo: float
+ métodos:
  - añadirSaldo(importe: float): void
  - retirarSaldo(importe: float): void
  - getSaldo(): float
}

Clase Cliente {
+ atributos:
  - nombre: string
  - dirección: string
  - teléfono: string
  - correoElectrónico: string
+ métodos:
  - getNombre(): string
  - getDirección(): string
  - getTeléfono(): string
  - getCorreoElectrónico(): string
}

Clase Producto {
+ atributos:
  - nombre: string
  - descripción: string
  - precio: float
+ métodos:
  - getNombre(): string
  - getDescripción(): string
  - getPrecio(): float
}

Clase Venta {
+ atributos:
  - fecha: Date
  - cliente: Cliente
  - productos: List<Producto>
  - importeTotal: float
+ métodos:
  - getFecha(): Date
  - getCliente(): Cliente
  - getProductos(): List<Producto>
  - getImporteTotal(): float
}

Clase Gasto {
+ atributos:
  - fecha: Date
  - concepto: string
  - importe: float
+ métodos:
  - getFecha(): Date
  - getConcepto(): string
  - getImporte(): float
}

Clase Beneficio {
+ atributos:
  - fecha: Date
  - concepto: string
  - importe: float
+ métodos:
  - getFecha(): Date
  - getConcepto(): string
  - getImporte(): float
}

```

Explicación del código:

* La clase SistemaContable es la clase principal del sistema contable. Es responsable de gestionar las cuentas del sistema, registrar las ventas y los gastos, y calcular los beneficios.
* La clase Cuenta representa las cuentas del sistema contable. Cada cuenta tiene un saldo, que puede ser positivo o negativo.
* La clase Cliente representa a los clientes del sistema contable. Cada cliente tiene un nombre, dirección, teléfono y correo electrónico.
* La clase Producto representa los productos que se venden en el sistema contable. Cada producto tiene un nombre, descripción y precio.
* La clase Venta representa las ventas que se realizan en el sistema contable. Cada venta tiene una fecha, un cliente y una lista de productos.
* La clase Gasto representa los gastos que se incurren en el sistema contable. Cada gasto tiene una fecha, un concepto y un importe.
* La clase Beneficio representa los beneficios que se obtienen en el sistema contable. Cada beneficio tiene una fecha, un concepto y un importe.