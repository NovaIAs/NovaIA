```smalltalk
```
```smalltalk
{Una clase que representa una cuenta bancaria}.
```smalltalk
CuentaBancaria subclass: [ CuentaBancaria ]
instanceVariableNames: 'titular balance'
poolDictionaries: ''
category: 'Bancos'

{Constructor de la clase CuentaBancaria}.
```smalltalk
initialize: titular inicial
	"Crea una nueva cuenta bancaria con el titular especificado y el balance inicial".
	super initialize.
	titular := titular.
	balance := inicial.

{Métodos de consulta}.
```smalltalk
titular
	"Devuelve el titular de la cuenta bancaria".
	^titular.

balance
	"Devuelve el balance de la cuenta bancaria".
	^balance.

{Métodos de operación}.
```smalltalk
depositar: monto
	"Deposita el monto especificado en la cuenta bancaria".
	balance := balance + monto.

retirar: monto
	"Retira el monto especificado de la cuenta bancaria".
	balance := balance - monto si: [balance >= monto].

{Representación de la cuenta bancaria}.
```smalltalk
imprimir
	"Imprime la información de la cuenta bancaria en la consola".
	Transcript show: 'Cuenta bancaria de ', titular.
	Transcript show: 'Balance: ', balance.
```
```smalltalk
{Una clase que representa un banco}.
```smalltalk
Banco subclass: [ Banco ]
instanceVariableNames: 'clientes cuentas'
poolDictionaries: ''
category: 'Bancos'

{Constructor de la clase Banco}.
```smalltalk
initialize
	"Crea un nuevo banco".
	super initialize.
	clientes := OrderedCollection new.
	cuentas := Dictionary new.

{Métodos de operación}.
```smalltalk
abrirCuenta: titular inicial
	"Abre una nueva cuenta bancaria para el titular especificado con el balance inicial".
	nuevaCuenta := CuentaBancaria new initialize: titular inicial.
	cuentas at: nuevaCuenta put: titular.
	clientes add: titular.

cerrarCuenta: cuenta
	"Cierra la cuenta bancaria especificada".
	titular := cuentas at: cuenta.
	clientes remove: titular.
	cuentas removeKey: cuenta.

consultarBalance: cuenta
	"Devuelve el balance de la cuenta bancaria especificada".
	^cuentas at: cuenta balance.

depositar: monto cuenta
	"Deposita el monto especificado en la cuenta bancaria especificada".
	cuentas at: cuenta depositar: monto.

retirar: monto cuenta
	"Retira el monto especificado de la cuenta bancaria especificada".
	cuentas at: cuenta retirar: monto.

{Representación del banco}.
```smalltalk
imprimir
	"Imprime la información del banco en la consola".
	Transcript show: 'Banco', self name.
	Transcript show: 'Clientes: ', clientes.
	Transcript show: 'Cuentas: ', cuentas.
```
```smalltalk
{Una clase que representa un cliente de un banco}.
```smalltalk
Cliente subclass: [ Cliente ]
instanceVariableNames: 'nombre'
poolDictionaries: ''
category: 'Bancos'

{Constructor de la clase Cliente}.
```smalltalk
initialize: nombre
	"Crea un nuevo cliente con el nombre especificado".
	super initialize.
	nombre := nombre.

{Métodos de consulta}.
```smalltalk
nombre
	"Devuelve el nombre del cliente".
	^nombre.

{Representación del cliente}.
```smalltalk
imprimir
	"Imprime la información del cliente en la consola".
	Transcript show: 'Cliente: ', nombre.
```
```smalltalk
{Una clase que representa una transacción bancaria}.
```smalltalk
Transaccion subclass: [ Transaccion ]
instanceVariableNames: 'cuenta monto tipo'
poolDictionaries: ''
category: 'Bancos'

{Constructor de la clase Transaccion}.
```smalltalk
initialize: cuenta monto tipo
	"Crea una nueva transacción con la cuenta, el monto y el tipo especificados".
	super initialize.
	cuenta := cuenta.
	monto := monto.
	tipo := tipo.

{Métodos de consulta}.
```smalltalk
cuenta
	"Devuelve la cuenta de la transacción".
	^cuenta.

monto
	"Devuelve el monto de la transacción".
	^monto.

tipo
	"Devuelve el tipo de la transacción".
	^tipo.

{Representación de la transacción}.
```smalltalk
imprimir
	"Imprime la información de la transacción en la consola".
	Transcript show: 'Transacción: ', cuenta.
	Transcript show: 'Monto: ', monto.
	Transcript show: 'Tipo: ', tipo.
```

Ahora, para utilizar estas clases, puedes crear un nuevo script en Smalltalk y escribir código como el siguiente:

```smalltalk
{Crea un nuevo banco llamado "Banco del Pueblo"}
banco := Banco new name: 'Banco del Pueblo'.

{Crea un nuevo cliente llamado "Juan Pérez"}
cliente := Cliente new initialize: 'Juan Pérez'.

{Abre una nueva cuenta bancaria para el cliente en el banco}
banco abrirCuenta: cliente inicial: 1000.

{Deposita 500 en la cuenta del cliente}
banco depositar: 500 cuenta: cliente.

{Retira 300 de la cuenta del cliente}
banco retirar: 300 cuenta: cliente.

{Imprime la información del banco, el cliente y la cuenta}
banco imprimir.
cliente imprimir.
banco cuentas at: cliente imprimir.

{Crea una nueva transacción para registrar el depósito}

deposito := Transaccion new initialize: banco cuentas at: cliente monto: 500 tipo: 'Depósito'.
deposito imprimir.

{Crea una nueva transacción para registrar el retiro}

retiro := Transaccion new initialize: banco cuentas at: cliente monto: 300 tipo: 'Retiro'.
retiro imprimir.
```

Este código creará un nuevo banco, un nuevo cliente, abrirá una nueva cuenta bancaria para el cliente, depositará y retirará dinero de la cuenta, e imprimirá la información del banco, el cliente, la cuenta y las transacciones.