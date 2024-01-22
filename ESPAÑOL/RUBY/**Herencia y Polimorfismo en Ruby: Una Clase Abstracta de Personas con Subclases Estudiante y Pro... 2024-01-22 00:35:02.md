```ruby
# Crear una clase llamada 'Persona' con atributos para nombre, edad y género.

class Persona
  def initialize(nombre, edad, género)
    @nombre = nombre
    @edad = edad
    @género = género
  end

  def saludar
    puts "Hola, mi nombre es #{@nombre} y tengo #{@edad} años."
  end
end

# Crear una clase llamada 'Estudiante' que hereda de la clase 'Persona'.

class Estudiante < Persona
  def initialize(nombre, edad, género, matricula)
    super(nombre, edad, género)
    @matricula = matricula
  end

  def estudiar
    puts "Estoy estudiando para mis exámenes."
  end
end

# Crear una clase llamada 'Profesor' que hereda de la clase 'Persona'.

class Profesor < Persona
  def initialize(nombre, edad, género, asignatura)
    super(nombre, edad, género)
    @asignatura = asignatura
  end

  def enseñar
    puts "Estoy enseñando la asignatura de #{@asignatura}."
  end
end

# Crear un objeto de la clase 'Persona' llamado 'juan'.

juan = Persona.new("Juan", 20, "Masculino")

# Crear un objeto de la clase 'Estudiante' llamado 'maría'.

maría = Estudiante.new("María", 18, "Femenino", "123456")

# Crear un objeto de la clase 'Profesor' llamado 'pedro'.

pedro = Profesor.new("Pedro", 30, "Masculino", "Matemáticas")

# Llamar al método 'saludar' en el objeto 'juan'.

juan.saludar

# Llamar al método 'estudiar' en el objeto 'maría'.

maría.estudiar

# Llamar al método 'enseñar' en el objeto 'pedro'.

pedro.enseñar
```

Explicación del código:

* **Clase 'Persona':** Define una clase llamada 'Persona' con tres atributos: nombre, edad y género. También define un método llamado 'saludar' que imprime un mensaje de saludo con el nombre y la edad de la persona.
* **Clase 'Estudiante':** Es una clase hija de la clase 'Persona'. Hereda todos los atributos y métodos de la clase 'Persona', y además define un nuevo atributo llamado 'matricula'. También define un nuevo método llamado 'estudiar' que imprime un mensaje que indica que el estudiante está estudiando.
* **Clase 'Profesor':** Es una clase hija de la clase 'Persona'. Hereda todos los atributos y métodos de la clase 'Persona', y además define un nuevo atributo llamado 'asignatura'. También define un nuevo método llamado 'enseñar' que imprime un mensaje que indica que el profesor está enseñando una asignatura.
* **Objetos 'juan', 'maría' y 'pedro':** Se crean tres objetos de las clases 'Persona', 'Estudiante' y 'Profesor', respectivamente. Estos objetos se utilizan para llamar a los métodos 'saludar', 'estudiar' y 'enseñar'.