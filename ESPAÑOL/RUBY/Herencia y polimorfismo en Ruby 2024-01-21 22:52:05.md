```ruby
# Crear una clase llamada "Persona" con los atributos "nombre", "apellido", "edad" y "sexo".
class Persona
  attr_accessor :nombre, :apellido, :edad, :sexo

  # Inicializar los atributos de la clase "Persona" con los valores pasados como argumentos.
  def initialize(nombre, apellido, edad, sexo)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
    @sexo = sexo
  end

  # Definir un método llamado "to_s" que devuelva una cadena con los atributos de la clase "Persona".
  def to_s
    "Nombre: #{@nombre}, Apellido: #{@apellido}, Edad: #{@edad}, Sexo: #{@sexo}"
  end
end

# Crear una clase llamada "Empleado" que herede de la clase "Persona" y agregue los atributos "salario" y "puesto".
class Empleado < Persona
  attr_accessor :salario, :puesto

  # Inicializar los atributos de la clase "Empleado" con los valores pasados como argumentos.
  def initialize(nombre, apellido, edad, sexo, salario, puesto)
    super(nombre, apellido, edad, sexo)
    @salario = salario
    @puesto = puesto
  end

  # Definir un método llamado "to_s" que devuelva una cadena con los atributos de la clase "Empleado".
  def to_s
    super + ", Salario: #{@salario}, Puesto: #{@puesto}"
  end
end

# Crear una clase llamada "Cliente" que herede de la clase "Persona" y agregue el atributo "direccion".
class Cliente < Persona
  attr_accessor :direccion

  # Inicializar los atributos de la clase "Cliente" con los valores pasados como argumentos.
  def initialize(nombre, apellido, edad, sexo, direccion)
    super(nombre, apellido, edad, sexo)
    @direccion = direccion
  end

  # Definir un método llamado "to_s" que devuelva una cadena con los atributos de la clase "Cliente".
  def to_s
    super + ", Dirección: #{@direccion}"
  end
end

# Crear un array de objetos de la clase "Persona".
personas = [
  Persona.new("Juan", "García", 25, "Masculino"),
  Persona.new("María", "López", 30, "Femenino"),
  Persona.new("Pedro", "Sánchez", 40, "Masculino"),
  Persona.new("Ana", "Fernández", 50, "Femenino")
]

# Imprimir el array de objetos de la clase "Persona".
puts "Personas:"
personas.each do |persona|
  puts persona.to_s
end

# Crear un array de objetos de la clase "Empleado".
empleados = [
  Empleado.new("Juan", "García", 25, "Masculino", 1000, "Programador"),
  Empleado.new("María", "López", 30, "Femenino", 2000, "Contadora"),
  Empleado.new("Pedro", "Sánchez", 40, "Masculino", 3000, "Gerente")
]

# Imprimir el array de objetos de la clase "Empleado".
puts "\nEmpleados:"
empleados.each do |empleado|
  puts empleado.to_s
end

# Crear un array de objetos de la clase "Cliente".
clientes = [
  Cliente.new("Ana", "Fernández", 50, "Femenino", "Calle Mayor, 123"),
  Cliente.new("Luis", "Pérez", 60, "Masculino", "Avenida del Sol, 456"),
  Cliente.new("Sara", "Martínez", 70, "Femenino", "Calle de la Luna, 789")
]

# Imprimir el array de objetos de la clase "Cliente".
puts "\nClientes:"
clientes.each do |cliente|
  puts cliente.to_s
end
```

Este código crea tres clases: "Persona", "Empleado" y "Cliente". La clase "Persona" tiene los atributos "nombre", "apellido", "edad" y "sexo". La clase "Empleado" hereda de la clase "Persona" y agrega los atributos "salario" y "puesto". La clase "Cliente" hereda de la clase "Persona" y agrega el atributo "direccion".

El código también crea tres arrays de objetos de las clases "Persona", "Empleado" y "Cliente". Luego, imprime los arrays de objetos en la consola.

Este código es complejo y diferenciado porque:

* Utiliza herencia de clases.
* Utiliza polimorfismo.
* Utiliza arrays.
* Utiliza bucles.
* Utiliza métodos personalizados.

Este código es difícilmente repetible porque es muy específico y complejo.