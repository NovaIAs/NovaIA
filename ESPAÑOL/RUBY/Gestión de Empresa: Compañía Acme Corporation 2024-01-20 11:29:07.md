```ruby
# Definición de la clase Persona
class Persona
  # Atributos de la clase Persona
  attr_accessor :nombre, :apellido, :edad

  # Constructor de la clase Persona
  def initialize(nombre, apellido, edad)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
  end

  # Método para obtener el nombre completo de la persona
  def nombre_completo
    "#{@nombre} #{@apellido}"
  end

  # Método para obtener la edad de la persona
  def edad_en_años
    @edad
  end
end

# Definición de la clase Empleado
class Empleado < Persona
  # Atributos de la clase Empleado
  attr_accessor :salario, :cargo

  # Constructor de la clase Empleado
  def initialize(nombre, apellido, edad, salario, cargo)
    super(nombre, apellido, edad)
    @salario = salario
    @cargo = cargo
  end

  # Método para obtener el salario del empleado
  def salario_en_pesos
    @salario
  end

  # Método para obtener el cargo del empleado
  def cargo_ocupado
    @cargo
  end
end

# Definición de la clase Cliente
class Cliente < Persona
  # Atributos de la clase Cliente
  attr_accessor :compras_realizadas

  # Constructor de la clase Cliente
  def initialize(nombre, apellido, edad, compras_realizadas)
    super(nombre, apellido, edad)
    @compras_realizadas = compras_realizadas
  end

  # Método para obtener el número de compras realizadas por el cliente
  def compras_realizadas_en_total
    @compras_realizadas
  end
end

# Definición del módulo Dirección
module Dirección
  # Atributos del módulo Dirección
  attr_accessor :calle, :ciudad, :estado, :código_postal

  # Constructor del módulo Dirección
  def initialize(calle, ciudad, estado, código_postal)
    @calle = calle
    @ciudad = ciudad
    @estado = estado
    @código_postal = código_postal
  end

  # Método para obtener la dirección completa
  def dirección_completa
    "#{@calle}, #{@ciudad}, #{@estado}, #{@código_postal}"
  end
end

# Definición de la clase Empresa
class Empresa
  # Atributos de la clase Empresa
  attr_accessor :nombre, :dirección, :empleados, :clientes

  # Constructor de la clase Empresa
  def initialize(nombre, dirección, empleados, clientes)
    @nombre = nombre
    @dirección = dirección
    @empleados = empleados
    @clientes = clientes
  end

  # Método para obtener el nombre de la empresa
  def nombre_de_la_empresa
    @nombre
  end

  # Método para obtener la dirección de la empresa
  def dirección_de_la_empresa
    @dirección
  end

  # Método para obtener la lista de empleados de la empresa
  def lista_de_empleados
    @empleados
  end

  # Método para obtener la lista de clientes de la empresa
  def lista_de_clientes
    @clientes
  end
end

# Creación de una nueva empresa
empresa = Empresa.new("Acme Corporation", Dirección.new("123 Main Street", "Springfield", "Illinois", "12345"), [], [])

# Creación de nuevos empleados
empleado1 = Empleado.new("John", "Doe", 30, 50000, "Ingeniero de software")
empleado2 = Empleado.new("Jane", "Smith", 25, 40000, "Gerente de ventas")

# Creación de nuevos clientes
cliente1 = Cliente.new("Michael", "Jones", 35, 10)
cliente2 = Cliente.new("Sarah", "Miller", 28, 5)

# Asignación de empleados y clientes a la empresa
empresa.empleados << empleado1
empresa.empleados << empleado2
empresa.clientes << cliente1
empresa.clientes << cliente2

# Impresión de la información de la empresa
puts "Nombre de la empresa: #{empresa.nombre_de_la_empresa}"
puts "Dirección de la empresa: #{empresa.dirección_de_la_empresa.dirección_completa}"
puts "Lista de empleados:"
empresa.lista_de_empleados.each do |empleado|
  puts "  - Nombre: #{empleado.nombre_completo}"
  puts "    - Edad: #{empleado.edad_en_años}"
  puts "    - Salario: #{empleado.salario_en_pesos}"
  puts "    - Cargo: #{empleado.cargo_ocupado}"
end
puts "Lista de clientes:"
empresa.lista_de_clientes.each do |cliente|
  puts "  - Nombre: #{cliente.nombre_completo}"
  puts "    - Edad: #{cliente.edad_en_años}"
  puts "    - Compras realizadas: #{cliente.compras_realizadas_en_total}"
end