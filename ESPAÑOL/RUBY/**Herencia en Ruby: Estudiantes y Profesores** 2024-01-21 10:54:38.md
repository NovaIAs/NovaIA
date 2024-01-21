```ruby
# Crear una clase llamada "Persona" con los atributos "nombre", "edad" y "sexo".

class Persona
  attr_accessor :nombre, :edad, :sexo

  # Método de inicialización de la clase "Persona".

  def initialize(nombre, edad, sexo)
    @nombre = nombre
    @edad = edad
    @sexo = sexo
  end

  # Método para obtener la información de la persona.

  def get_informacion
    "Nombre: #{@nombre}, Edad: #{@edad}, Sexo: #{@sexo}"
  end
end

# Crear una clase llamada "Estudiante" que herede de la clase "Persona".

class Estudiante < Persona
  # Atributos adicionales de la clase "Estudiante".
  
  attr_accessor :matricula, :carrera	

  # Método de inicialización de la clase "Estudiante".

  def initialize(nombre, edad, sexo, matricula, carrera)
    super(nombre, edad, sexo) 
    @matricula = matricula
    @carrera = carrera
  end

  # Método para obtener la información del estudiante.

  def get_informacion
    super + ", Matrícula: #{@matricula}, Carrera: #{@carrera}"
  end
end

# Crear una clase llamada "Profesor" que herede de la clase "Persona".

class Profesor < Persona
  # Atributos adicionales de la clase "Profesor".

  attr_accessor :codigo_profesor, :departamento

  # Método de inicialización de la clase "Profesor".

  def initialize(nombre, edad, sexo, codigo_profesor, departamento)
    super(nombre, edad, sexo)
    @codigo_profesor = codigo_profesor
    @departamento = departamento
  end

  # Método para obtener la información del profesor.

  def get_informacion
    super + ", Código de Profesor: #{@codigo_profesor}, Departamento: #{@departamento}"
  end
end

# Crear un objeto de la clase "Estudiante".

estudiante1 = Estudiante.new("Juan", 20, "Masculino", "123456", "Ingeniería en Sistemas")

# Crear un objeto de la clase "Profesor".

profesor1 = Profesor.new("María", 35, "Femenino", "111111", "Ciencias de la Computación")

# Imprimir la información del estudiante y del profesor.

puts estudiante1.get_informacion
puts profesor1.get_informacion
```

Explicación del código:

* Se crean tres clases: "Persona", "Estudiante" y "Profesor". La clase "Persona" es la clase base y las clases "Estudiante" y "Profesor" heredan de ella.
* La clase "Persona" tiene los atributos "nombre", "edad" y "sexo".
* La clase "Estudiante" tiene los atributos adicionales "matricula" y "carrera".
* La clase "Profesor" tiene los atributos adicionales "codigo_profesor" y "departamento".
* Cada clase tiene un método de inicialización que inicializa los atributos de la clase.
* Cada clase tiene un método llamado "get_informacion" que devuelve una cadena con la información de la persona, el estudiante o el profesor.
* Se crean dos objetos de la clase "Estudiante" y "Profesor".
* Se imprime la información del estudiante y del profesor.