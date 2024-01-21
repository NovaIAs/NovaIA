```ruby
require 'csv'
require 'json'
require 'open-uri'

# Obtener datos de un archivo CSV
datos_csv = CSV.read('data.csv', headers: true)

# Convertir los datos CSV a JSON
datos_json = datos_csv.to_json

# Escribir los datos JSON en un archivo
File.open('data.json', 'w') do |f|
  f.write(datos_json)
end

# Obtener datos de una API externa
url = 'https://jsonplaceholder.typicode.com/posts'
datos_api = JSON.parse(open(url).read)

# Filtrar los datos de la API
datos_filtrados = datos_api.select { |p| p['userId'] == 1 }

# Mostrar los datos filtrados en la consola
puts datos_filtrados

# Crear un objeto de clase
class Persona
  attr_accessor :nombre, :edad

  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  def hablar
    puts "Hola, mi nombre es #{@nombre} y tengo #{@edad} años."
  end
end

# Crear una instancia de la clase
persona1 = Persona.new('Juan', 25)

# Llamar al método hablar de la instancia
persona1.hablar

# Crear un módulo
module Matematicas
  def suma(a, b)
    a + b
  end

  def resta(a, b)
    a - b
  end

  def multiplicacion(a, b)
    a * b
  end

  def division(a, b)
    a / b
  end
end

# Incluir el módulo en una clase
class Calculadora
  include Matematicas

  def calcular(operacion, a, b)
    case operacion
    when 'suma'
      suma(a, b)
    when 'resta'
      resta(a, b)
    when 'multiplicacion'
      multiplicacion(a, b)
    when 'division'
      division(a, b)
    else
      puts 'Operación no válida.'
    end
  end
end

# Crear una instancia de la clase Calculadora
calculadora = Calculadora.new

# Llamar al método calcular de la instancia
resultado = calculadora.calcular('suma', 10, 5)