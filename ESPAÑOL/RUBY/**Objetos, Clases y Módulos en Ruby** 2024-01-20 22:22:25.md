```ruby
# Definimos una clase llamada "Coche" con atributos y métodos.
class Coche
  attr_accessor :marca, :modelo, :anio, :precio

  def initialize(marca, modelo, anio, precio)
    @marca = marca
    @modelo = modelo
    @anio = anio
    @precio = precio
  end

  def acelerar
    puts "El coche está acelerando."
  end

  def frenar
    puts "El coche está frenando."
  end

  def girar_izquierda
    puts "El coche está girando a la izquierda."
  end

  def girar_derecha
    puts "El coche está girando a la derecha."
  end
end

# Creamos un objeto de la clase "Coche" con los valores de los atributos.
coche1 = Coche.new("Toyota", "Camry", 2023, 30000)

# Mostramos los valores de los atributos del objeto "coche1".
puts "Marca: #{coche1.marca}"
puts "Modelo: #{coche1.modelo}"
puts "Año: #{coche1.anio}"
puts "Precio: #{coche1.precio}"

# Llamamos a los métodos del objeto "coche1" para simular acciones.
coche1.acelerar
coche1.frenar
coche1.girar_izquierda
coche1.girar_derecha

# Definimos una clase llamada "Persona" con atributos y métodos.
class Persona
  attr_accessor :nombre, :apellido, :edad, :genero

  def initialize(nombre, apellido, edad, genero)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
    @genero = genero
  end

  def caminar
    puts "La persona está caminando."
  end

  def correr
    puts "La persona está corriendo."
  end

  def saltar
    puts "La persona está saltando."
  end

  def hablar
    puts "La persona está hablando."
  end
end

# Creamos un objeto de la clase "Persona" con los valores de los atributos.
persona1 = Persona.new("Juan", "Pérez", 25, "Masculino")

# Mostramos los valores de los atributos del objeto "persona1".
puts "Nombre: #{persona1.nombre}"
puts "Apellido: #{persona1.apellido}"
puts "Edad: #{persona1.edad}"
puts "Género: #{persona1.genero}"

# Llamamos a los métodos del objeto "persona1" para simular acciones.
persona1.caminar
persona1.correr
persona1.saltar
persona1.hablar

# Definimos un módulo llamado "Matematicas" con métodos matemáticos.
module Matematicas
  def sumar(a, b)
    a + b
  end

  def restar(a, b)
    a - b
  end

  def multiplicar(a, b)
    a * b
  end

  def dividir(a, b)
    a / b
  end
end

# Incluimos el módulo "Matematicas" en la clase "Coche".
class Coche
  include Matematicas
end

# Creamos un objeto de la clase "Coche" con los valores de los atributos.
coche2 = Coche.new("Nissan", "Altima", 2022, 25000)

# Llamamos a los métodos matemáticos del objeto "coche2" usando el módulo "Matematicas".
puts "Suma: #{coche2.sumar(10, 20)}"
puts "Resta: #{coche2.restar(20, 10)}"
puts "Multiplicación: #{coche2.multiplicar(10, 20)}"
puts "División: #{coche2.dividir(20, 10)}"
```

Explicación del código:

1. Definimos dos clases, "Coche" y "Persona", con atributos y métodos específicos. Los atributos son las propiedades de los objetos, como la marca, el modelo y el año del coche, o el nombre, el apellido y la edad de la persona. Los métodos son las acciones que los objetos pueden realizar, como acelerar, frenar o girar en el caso del coche, o caminar, correr o saltar en el caso de la persona.

2. Creamos objetos de las clases "Coche" y "Persona" con valores específicos para los atributos. Estos objetos representan instancias concretas de las clases, con sus propias características.

3. Mostramos los valores de los atributos de los objetos usando el método `puts`. Esto nos permite ver el estado de los objetos en un momento determinado.

4. Llamamos a los métodos de los objetos para simular acciones. Por ejemplo, podemos llamar al método `acelerar` del objeto `coche1` para simular que el coche está acelerando.

5. Definimos un módulo llamado "Matematicas" con métodos matemáticos comunes. Los módulos nos permiten agrupar métodos relacionados en una unidad lógica.

6. Incluimos el módulo "Matematicas" en la clase "Coche" usando la palabra clave `include`. Esto nos permite acceder a los métodos matemáticos del módulo dentro de la clase "Coche".

7. Creamos otro objeto de la clase "Coche" y llamamos a los métodos matemáticos del módulo "Matematicas" usando el objeto `coche2`. Esto demuestra cómo podemos usar módulos para añadir funcionalidad a las clases.