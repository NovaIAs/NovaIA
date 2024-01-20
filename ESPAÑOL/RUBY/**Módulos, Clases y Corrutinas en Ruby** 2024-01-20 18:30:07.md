```ruby
# Definición de módulos y clases.

module MiModulo
  def self.funcion_util
    puts "Soy una función útil dentro del módulo MiModulo."
  end
end

class MiClase
  include MiModulo # Incluir el módulo MiModulo en la clase MiClase.

  def initialize
    @atributo = "Valor inicial"
  end

  def obtener_atributo
    @atributo
  end

  def establecer_atributo(nuevo_valor)
    @atributo = nuevo_valor
  end
end

# Definición de bloques.

bloque_simple = -> { puts "Soy un bloque simple." }

bloque_con_parámetros = -> (nombre) { puts "Hola #{nombre}!" }

# Definición de corrutinas.

def corrutina_simple
  3.times do |i|
    yield i
  end
end

# Uso de los módulos, clases, bloques y corrutinas.

MiModulo.funcion_util

objeto_clase = MiClase.new
puts objeto_clase.obtener_atributo
objeto_clase.establecer_atributo "Nuevo valor"
puts objeto_clase.obtener_atributo

bloque_simple.call
bloque_con_parámetros.call "Juan"

(1..5).each do |n|
  puts n * 2
end

corrutina_simple do |i|
  puts "El valor actual es #{i}"
end
```

Explicación del código:

* Módulos y clases: Los módulos y las clases nos permiten organizar y estructurar nuestro código en partes reutilizables. En este caso, el módulo MiModulo define una función útil que se puede utilizar en otras partes del código. La clase MiClase incluye el módulo MiModulo, lo que significa que hereda su funcionalidad.
* Bloques: Los bloques son una forma de definir código que se puede pasar como argumento a métodos o funciones. En este caso, hemos definido un bloque simple y un bloque con parámetros.
* Corrutinas: Las corrutinas son una forma de crear código que puede pausar y reanudar su ejecución en diferentes momentos. En este caso, hemos definido una corrutina simple que imprime los números del 1 al 3.
* Uso de los módulos, clases, bloques y corrutinas: En la última parte del código, utilizamos los módulos, clases, bloques y corrutinas que hemos definido anteriormente.