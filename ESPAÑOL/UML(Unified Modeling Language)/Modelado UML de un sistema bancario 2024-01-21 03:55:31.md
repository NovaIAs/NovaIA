```
Diagrama de clases UML

Clase: Persona

Atributos:
- id: int
- nombre: str
- apellido: str
- fecha_nacimiento: date
- telefono: str
- correo_electronico: str

Métodos:
- constructor(id, nombre, apellido, fecha_nacimiento, telefono, correo_electronico)
- getId() -> int
- getNombre() -> str
- getApellido() -> str
- getFechaNacimiento() -> date
- getTelefono() -> str
- getCorreoElectronico() -> str
- __str__() -> str

Clase: Cuenta

Atributos:
- id: int
- numero_cuenta: str
- saldo: float
- persona: Persona
- estado: str

Métodos:
- constructor(id, numero_cuenta, saldo, persona, estado)
- getId() -> int
- getNumeroCuenta() -> str
- getSaldo() -> float
- getPersona() -> Persona
- getEstado() -> str
- __str__() -> str

Clase: Transaccion

Atributos:
- id: int
- fecha: date
- hora: time
- tipo: str
- monto: float
- cuenta: Cuenta
- comentario: str

Métodos:
- constructor(id, fecha, hora, tipo, monto, cuenta, comentario)
- getId() -> int
- getFecha() -> date
- getHora() -> time
- getTipo() -> str
- getMonto() -> float
- getCuenta() -> Cuenta
- getComentario() -> str
- __str__() -> str

Diagrama de casos de uso UML

Actor: Cliente

Caso de uso: Retirar dinero

Descripción: El cliente ingresa a su cuenta bancaria y solicita un retiro de dinero.

Preconditions:
- El cliente debe estar registrado en el banco.
- El cliente debe tener una cuenta bancaria activa.
- La cuenta bancaria debe tener saldo suficiente para cubrir el retiro.

Flow of events:
1. El cliente selecciona la opción "Retirar dinero" en el menú de su cuenta bancaria.
2. El cliente ingresa el monto que desea retirar.
3. El sistema verifica que el cliente tenga saldo suficiente en su cuenta para cubrir el retiro.
4. Si el cliente tiene saldo suficiente, el sistema retira el monto solicitado de la cuenta bancaria del cliente.
5. El sistema imprime un recibo del retiro.

Postconditions:
- El cliente ha retirado el monto solicitado de su cuenta bancaria.
- El sistema ha impreso un recibo del retiro.
- El saldo de la cuenta bancaria del cliente ha sido actualizado.

Diagrama de actividades UML

Actividad: Proceso de retiro de dinero

Descripción: Este diagrama muestra el proceso de retiro de dinero desde una cuenta bancaria.

Start: El cliente ingresa a su cuenta bancaria.

Actividades:
1. El cliente selecciona la opción "Retirar dinero" en el menú de su cuenta bancaria.
2. El cliente ingresa el monto que desea retirar.
3. El sistema verifica que el cliente tenga saldo suficiente en su cuenta para cubrir el retiro.
4. Si el cliente tiene saldo suficiente, el sistema retira el monto solicitado de la cuenta bancaria del cliente.
5. El sistema imprime un recibo del retiro.

End: El cliente ha retirado el monto solicitado de su cuenta bancaria.

Diagrama de secuencia UML

Secuencia: Proceso de retiro de dinero

Participantes:
- Cliente
- Sistema

Mensajes:
1. Cliente -> Sistema: Solicitar retiro de dinero
2. Sistema -> Cliente: Verificar saldo de cuenta
3. Sistema -> Cliente: Imprimir recibo de retiro

Flujo de mensajes:
1. El cliente envía una solicitud de retiro de dinero al sistema.
2. El sistema verifica el saldo de la cuenta del cliente.
3. Si el cliente tiene saldo suficiente, el sistema retira el monto solicitado de la cuenta bancaria del cliente.
4. El sistema imprime un recibo del retiro.
```

Este código UML es un ejemplo de cómo se puede utilizar este lenguaje para modelar un sistema de gestión de cuentas bancarias. El diagrama de clases define las clases Persona, Cuenta y Transacción, que representan los principales conceptos del sistema. El diagrama de casos de uso define el caso de uso Retirar dinero, que muestra cómo un cliente puede retirar dinero de su cuenta bancaria. El diagrama de actividades define el proceso de retiro de dinero, que muestra los pasos que se siguen para completar el retiro. Finalmente, el diagrama de secuencia define la secuencia de mensajes que se intercambian entre el cliente y el sistema durante el proceso de retiro de dinero.