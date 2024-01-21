**Diagrama de Clases Complejo en UML**

```
+----------------+
| Clase Persona |
+----------------+
| - id: int |
| - nombre: string |
| - apellido: string |
| - fecha_nacimiento: date |
| - correo_electronico: string |
| - telefono: string |
| - direccion: string |
| - ciudad: string |
| - pais: string |
| - codigo_postal: string |
+----------------+

+-----------------------+
| Clase Cuenta Bancaria |
+-----------------------+
| - id: int |
| - numero_cuenta: string |
| - saldo: float |
| - tipo_cuenta: string |
| - persona: Persona |
+-----------------------+

+----------------+
| Clase Tarjeta |
+----------------+
| - id: int |
| - numero_tarjeta: string |
| - fecha_expiracion: date |
| - CVV: string |
| - cuenta_bancaria: CuentaBancaria |
+----------------+

+----------------------+
| Clase Transaccion |
+----------------------+
| - id: int |
| - fecha_transaccion: date |
| - monto: float |
| - tipo_transaccion: string |
| - tarjeta: Tarjeta |
+----------------------+

+-------------------------------------------------+
| Clase Historial de Transacciones de la Cuenta |
+-------------------------------------------------+
| - id: int |
| - fecha_inicio: date |
| - fecha_fin: date |
| - transacciones: List<Transaccion> |
| - cuenta_bancaria: CuentaBancaria |
+-------------------------------------------------+

+----------------+
| Clase Sucursal |
+----------------+
| - id: int |
| - nombre: string |
| - direccion: string |
| - ciudad: string |
| - pais: string |
| - codigo_postal: string |
+----------------+

+----------------------+
| Clase Empleado |
+----------------------+
| - id: int |
| - nombre: string |
| - apellido: string |
| - fecha_nacimiento: date |
| - correo_electronico: string |
| - telefono: string |
| - direccion: string |
| - ciudad: string |
| - pais: string |
| - codigo_postal: string |
| - sucursal: Sucursal |
+----------------------+

+----------------+
| Clase Cliente |
+----------------+
| - id: int |
| - nombre: string |
| - apellido: string |
| - fecha_nacimiento: date |
| - correo_electronico: string |
| - telefono: string |
| - direccion: string |
| - ciudad: string |
| - pais: string |
| - codigo_postal: string |
+----------------+

**Relaciones entre Clases**

* Persona es la superclase de Cliente y Empleado.
* CuentaBancaria tiene una relación uno-a-muchos con Tarjeta.
* Tarjeta tiene una relación uno-a-muchos con Transaccion.
* CuentaBancaria tiene una relación uno-a-uno con Historial de Transacciones de la Cuenta.
* Sucursal tiene una relación uno-a-muchos con Empleado.
* Cliente es una subclase de Persona.
* Empleado es una subclase de Persona.

**Explicación del Código**

El código anterior crea un diagrama de clases complejo en UML. El diagrama muestra las relaciones entre diferentes clases en un sistema bancario. Las clases incluyen Persona, CuentaBancaria, Tarjeta, Transaccion, Historial de Transacciones de la Cuenta, Sucursal, Empleado y Cliente.

El código comienza definiendo la clase Persona. La clase Persona tiene los siguientes atributos: id, nombre, apellido, fecha_nacimiento, correo_electronico, telefono, direccion, ciudad, pais y codigo_postal.

La siguiente clase definida es CuentaBancaria. La clase CuentaBancaria tiene los siguientes atributos: id, numero_cuenta, saldo, tipo_cuenta y persona. La relación entre CuentaBancaria y Persona es uno-a-muchos, lo que significa que una persona puede tener muchas cuentas bancarias.

La siguiente clase definida es Tarjeta. La clase Tarjeta tiene los siguientes atributos: id, numero_tarjeta, fecha_expiracion, CVV y cuenta_bancaria. La relación entre Tarjeta y CuentaBancaria es uno-a-muchos, lo que significa que una cuenta bancaria puede tener muchas tarjetas.

La siguiente clase definida es Transaccion. La clase Transaccion tiene los siguientes atributos: id, fecha_transaccion, monto, tipo_transaccion y tarjeta. La relación entre Transaccion y Tarjeta es uno-a-muchos, lo que significa que una tarjeta puede tener muchas transacciones.

La siguiente clase definida es Historial de Transacciones de la Cuenta. La clase Historial de Transacciones de la Cuenta tiene los siguientes atributos: id, fecha_inicio, fecha_fin, transacciones y cuenta_bancaria. La relación entre Historial de Transacciones de la Cuenta y CuentaBancaria es uno-a-uno, lo que significa que una cuenta bancaria puede tener un historial de transacciones.

La siguiente clase definida es Sucursal. La clase Sucursal tiene los siguientes atributos: id, nombre, direccion, ciudad, pais y codigo_postal.

La siguiente clase definida es Empleado. La clase Empleado tiene los siguientes atributos: id, nombre, apellido, fecha_nacimiento, correo_electronico, telefono, direccion, ciudad, pais, codigo_postal y sucursal. La relación entre Empleado y Sucursal es uno-a-muchos, lo que significa que una sucursal puede tener muchos empleados.

La última clase definida es Cliente. La clase Cliente tiene los siguientes atributos: id, nombre, apellido, fecha_nacimiento, correo_electronico, telefono, direccion, ciudad, pais y codigo_postal. La clase Cliente es una subclase de la clase Persona, lo que significa que hereda todos los atributos y métodos de la clase Persona.

El código finaliza definiendo las relaciones entre las clases. Las relaciones se definen utilizando los caracteres "->" y "<