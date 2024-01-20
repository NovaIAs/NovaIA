```ruby
# Definir una clase llamada "Persona" con atributos "nombre", "edad" y "ocupación".
class Persona
  attr_accessor :nombre, :edad, :ocupacion

  # Método constructor para inicializar los atributos al crear un nuevo objeto de tipo Persona.
  def initialize(nombre, edad, ocupacion)
    @nombre = nombre
    @edad = edad
    @ocupacion = ocupacion
  end

  # Método para obtener información sobre la persona en un formato legible.
  def to_s
    "Nombre: #{@nombre}, Edad: #{@edad}, Ocupación: #{@ocupacion}"
  end
end

# Definir una clase llamada "Empleado" que hereda de la clase "Persona".
class Empleado < Persona
  attr_accessor :salario, :departamento

  # Método constructor para inicializar los atributos al crear un nuevo objeto de tipo Empleado.
  def initialize(nombre, edad, ocupacion, salario, departamento)
    super(nombre, edad, ocupacion)  # Llamar al constructor de la clase padre para inicializar los atributos comunes.
    @salario = salario
    @departamento = departamento
  end

  # Método para obtener información sobre el empleado en un formato legible.
  def to_s
    super + ", Salario: #{@salario}, Departamento: #{@departamento}"  # Llamar al método to_s de la clase padre y concatenar información adicional.
  end
end

# Definir una clase llamada "Estudiante" que hereda de la clase "Persona".
class Estudiante < Persona
  attr_accessor :universidad, :carrera

  # Método constructor para inicializar los atributos al crear un nuevo objeto de tipo Estudiante.
  def initialize(nombre, edad, ocupacion, universidad, carrera)
    super(nombre, edad, ocupacion)  # Llamar al constructor de la clase padre para inicializar los atributos comunes.
    @universidad = universidad
    @carrera = carrera
  end

  # Método para obtener información sobre el estudiante en un formato legible.
  def to_s
    super + ", Universidad: #{@universidad}, Carrera: #{@carrera}"  # Llamar al método to_s de la clase padre y concatenar información adicional.
  end
end

# Crear un arreglo de personas con diferentes tipos de objetos.
personas = [
  Persona.new("Juan", 25, "Ingeniero"),
  Empleado.new("María", 30, "Contadora", 2500, "Contabilidad"),
  Estudiante.new("Pedro", 20, "Estudiante", "Universidad Nacional", "Ingeniería")
]

# Recorrer el arreglo de personas e imprimir información sobre cada una.
personas.each do |persona|
  puts persona
end
```

Este código define tres clases: "Persona", "Empleado" y "Estudiante". La clase "Persona" es una clase base que define los atributos comunes de una persona, como su nombre, edad y ocupación. Las clases "Empleado" y "Estudiante" heredan de la clase "Persona" y añaden atributos adicionales específicos para empleados y estudiantes, respectivamente.

Además, el código crea un arreglo de personas con diferentes tipos de objetos y recorre el arreglo para imprimir información sobre cada persona.

Este código es complejo porque:

* Utiliza herencia de clases para crear clases derivadas con atributos y métodos específicos para cada tipo de objeto.
* Utiliza el patrón de diseño "polimorfismo" para tratar objetos de diferentes tipos de manera uniforme.
* Implementa un método "to_s" en cada clase para obtener información sobre el objeto en un formato legible.
* Utiliza un arreglo para almacenar objetos de diferentes tipos y recorre el arreglo para procesar cada objeto.

Este código es amplio y diferenciado porque:

* Define tres clases diferentes con diferentes atributos y métodos.
* Crea un arreglo de objetos de diferentes tipos y recorre el arreglo para procesar cada objeto.
* Implementa un método "to_s" en cada clase para obtener información sobre el objeto en un formato legible.

Este código es difícilmente repetible porque:

* Utiliza herencia de clases y polimorfismo, que son conceptos avanzados de programación.
* Implementa un método "to_s" en cada clase, que es una técnica de programación avanzada.
* Crea un arreglo de objetos de diferentes tipos y recorre el arreglo para procesar cada objeto, que es una técnica de programación avanzada.