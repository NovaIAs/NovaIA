```ruby
# Definición de la clase "Cuenta Bancaria"
class CuentaBancaria
  # Método de clase para crear una nueva cuenta bancaria
  def self.crear(nombre, saldo_inicial)
    # Instancia una nueva CuentaBancaria
    cuenta = CuentaBancaria.new(nombre, saldo_inicial)

    # Persiste la cuenta en la base de datos
    cuenta.save

    # Devuelve la cuenta creada
    cuenta
  end

  # Atributos de la clase
  attr_accessor :nombre, :saldo

  # Constructor de la clase
  def initialize(nombre, saldo_inicial)
    @nombre = nombre
    @saldo = saldo_inicial
  end

  # Método de instancia para depositar dinero en la cuenta
  def depositar(monto)
    # Incrementa el saldo de la cuenta
    @saldo += monto

    # Actualiza la cuenta en la base de datos
    save
  end

  # Método de instancia para retirar dinero de la cuenta
  def retirar(monto)
    # Verifica si el monto a retirar es menor o igual al saldo
    if monto <= @saldo
      # Decrementa el saldo de la cuenta
      @saldo -= monto

      # Actualiza la cuenta en la base de datos
      save
    else
      # Genera una excepción si el monto a retirar es mayor al saldo
      raise ArgumentError, "El monto a retirar es mayor al saldo actual"
    end
  end

  # Método de instancia para obtener el saldo de la cuenta
  def saldo
    # Devuelve el saldo de la cuenta
    @saldo
  end

  # Método de instancia para persistir la cuenta en la base de datos
  def save
    # Simulación de la persistencia de la cuenta en una base de datos
  end
end

# Creación de una nueva cuenta bancaria
cuenta_ahorros = CuentaBancaria.crear("Cuenta de Ahorros", 1000)

# Depósito de dinero en la cuenta
cuenta_ahorros.depositar(500)

# Retiro de dinero de la cuenta
cuenta_ahorros.retirar(300)

# Consulta del saldo de la cuenta
saldo_actual = cuenta_ahorros.saldo

# Impresión del saldo de la cuenta en la consola
puts "Saldo actual: #{saldo_actual}"
```

Explicación del código:

1. **Definición de la clase "Cuenta Bancaria"**: Se define una clase `CuentaBancaria` que representa una cuenta bancaria con atributos de nombre y saldo.

2. **Método de clase para crear una nueva cuenta bancaria**: Se define un método de clase `crear` que crea una nueva cuenta bancaria con un nombre y saldo inicial especificados.

3. **Constructor de la clase**: El constructor de la clase `CuentaBancaria` inicializa los atributos de nombre y saldo con los valores proporcionados.

4. **Métodos de instancia para depositar y retirar dinero**: Se definen métodos de instancia `depositar` y `retirar` que permiten depositar y retirar dinero de la cuenta, respectively.

5. **Método de instancia para obtener el saldo de la cuenta**: Se define un método de instancia `saldo` que devuelve el saldo actual de la cuenta.

6. **Método de instancia para persistir la cuenta en la base de datos**: Se define un método de instancia `save` que simulación de la persistencia de la cuenta en una base de datos.

7. **Creación de una nueva cuenta bancaria**: Se crea una nueva cuenta de ahorros utilizando el método de clase `crear`.

8. **Depósito de dinero en la cuenta**: Se deposita una cantidad de dinero en la cuenta de ahorros utilizando el método `depositar`.

9. **Retiro de dinero de la cuenta**: Se retira una cantidad de dinero de la cuenta de ahorros utilizando el método `retirar`.

10. **Consulta del saldo de la cuenta**: Se consulta el saldo actual de la cuenta de ahorros utilizando el método `saldo`.

11. **Impresión del saldo de la cuenta en la consola**: Se imprime el saldo actual de la cuenta de ahorros en la consola.