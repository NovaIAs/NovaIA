```ruby
# Clase que representa un banco
class Banco

  # Constructor de la clase
  def initialize(nombre, direccion)
    @nombre = nombre
    @direccion = direccion
    @cuentas = []
  end

  # Agrega una cuenta al banco
  def agregar_cuenta(cuenta)
    @cuentas << cuenta
  end

  # Obtiene todas las cuentas del banco
  def obtener_cuentas
    @cuentas
  end

  # Obtiene el nombre del banco
  def obtener_nombre
    @nombre
  end

  # Obtiene la dirección del banco
  def obtener_direccion
    @direccion
  end

end

# Clase que representa una cuenta
class Cuenta

  # Constructor de la clase
  def initialize(numero, saldo)
    @numero = numero
    @saldo = saldo
  end

  # Aumenta el saldo de la cuenta
  def ingresar(monto)
    @saldo += monto
  end

  # Disminuye el saldo de la cuenta
  def retirar(monto)
    if monto <= @saldo
      @saldo -= monto
    end
  end

  # Obtiene el número de la cuenta
  def obtener_numero
    @numero
  end

  # Obtiene el saldo de la cuenta
  def obtener_saldo
    @saldo
  end

end

# Clase principal
class Main

  # Punto de entrada al programa
  def self.main
    # Crea un banco
    banco1 = Banco.new("Banco del Pueblo", "Calle 123")

    # Crea una cuenta
    cuenta1 = Cuenta.new("123456", 1000)

    # Agrega la cuenta al banco
    banco1.agregar_cuenta(cuenta1)

    # Obtiene el nombre del banco
    puts banco1.obtener_nombre

    # Obtiene la dirección del banco
    puts banco1.obtener_direccion

    # Obtiene todas las cuentas del banco
    cuentas = banco1.obtener_cuentas

    # Recorre las cuentas
    cuentas.each do |cuenta|
      # Obtiene el número de la cuenta
      puts cuenta.obtener_numero

      # Obtiene el saldo de la cuenta
      puts cuenta.obtener_saldo
    end

    # Ingresa 500 a la cuenta
    cuenta1.ingresar(500)

    # Obtiene el saldo de la cuenta
    puts cuenta1.obtener_saldo

    # Retira 300 de la cuenta
    cuenta1.retirar(300)

    # Obtiene el saldo de la cuenta
    puts cuenta1.obtener_saldo
  end

end

# Llama a la clase principal
Main.main
```

Explicación del código:

1. Se crea una clase `Banco` que representa un banco.
2. Se crea una clase `Cuenta` que representa una cuenta bancaria.
3. Se crea una clase `Main` que es la clase principal del programa.
4. En el método `main` de la clase `Main` se crea un banco y una cuenta, y se agrega la cuenta al banco.
5. Se obtienen el nombre y la dirección del banco, y se recorren las cuentas del banco para obtener el número y el saldo de cada cuenta.
6. Se ingresa 500 a la cuenta y se retira 300 de la cuenta.
7. Se obtiene el saldo de la cuenta después de las transacciones.