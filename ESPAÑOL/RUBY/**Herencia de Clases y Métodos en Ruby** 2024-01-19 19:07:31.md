```ruby
# Definimos una clase llamada "Persona"
class Persona
  # La clase "Persona" tiene dos atributos: "nombre" y "edad"
  attr_accessor :nombre, :edad

  # Definimos un método llamado "saludar" que imprime el nombre y la edad de la persona
  def saludar
    puts "Hola, mi nombre es #{nombre} y tengo #{edad} años"
  end
end

# Creamos una instancia de la clase "Persona" llamada "juan"
juan = Persona.new

# Establecemos el nombre y la edad de "juan"
juan.nombre = "Juan"
juan.edad = 25

# Llamamos al método "saludar" de "juan"
juan.saludar

# Definimos una clase llamada "Estudiante" que hereda de la clase "Persona"
class Estudiante < Persona
  # La clase "Estudiante" tiene un atributo adicional llamado "calificaciones"
  attr_accessor :calificaciones

  # Definimos un método llamado "estudiar" que imprime las calificaciones del estudiante
  def estudiar
    puts "Mis calificaciones son: #{calificaciones}"
  end
end

# Creamos una instancia de la clase "Estudiante" llamada "maria"
maria = Estudiante.new

# Establecemos el nombre, la edad y las calificaciones de "maria"
maria.nombre = "María"
maria.edad = 20
maria.calificaciones = [9, 8, 7]

# Llamamos al método "saludar" de "maria"
maria.saludar

# Llamamos al método "estudiar" de "maria"
maria.estudiar
```

Explicación:

* La primera línea del código define una clase llamada `Persona`. Una clase es una plantilla para crear objetos. Proporciona los atributos y métodos comunes a todos los objetos creados a partir de ella.
* En la clase `Persona`, definimos dos atributos: `nombre` y `edad`. Los atributos son variables que almacenan información sobre los objetos.
* También definimos un método llamado `saludar`. Un método es una función que puede ser llamada por los objetos de una clase. El método `saludar` imprime el nombre y la edad de la persona.
* Luego, creamos una instancia de la clase `Persona` llamada `juan`. Una instancia es un objeto creado a partir de una clase.
* Establecemos el nombre y la edad de `juan` usando los métodos `nombre=` y `edad=`.
* Llamamos al método `saludar` de `juan` usando el operador `.`. El operador `.` se utiliza para llamar a los métodos de los objetos.
* A continuación, definimos una clase llamada `Estudiante` que hereda de la clase `Persona`. La herencia es un mecanismo que permite a una clase heredar los atributos y métodos de otra clase.
* En la clase `Estudiante`, definimos un atributo adicional llamado `calificaciones`.
* También definimos un método llamado `estudiar` que imprime las calificaciones del estudiante.
* Creamos una instancia de la clase `Estudiante` llamada `maria`.
* Establecemos el nombre, la edad y las calificaciones de `maria` usando los métodos `nombre=`, `edad=` y `calificaciones=`.
* Llamamos al método `saludar` de `maria`.
* Llamamos al método `estudiar` de `maria`.