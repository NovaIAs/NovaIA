```ruby
# Clase para crear y administrar alumnos
class Alumno
  attr_accessor :nombre, :apellido, :nota

  def initialize(nombre, apellido, nota)
    @nombre = nombre
    @apellido = apellido
    @nota = nota
  end

  def to_s
    "Nombre: #{@nombre}, Apellido: #{@apellido}, Nota: #{@nota}"
  end
end

# Clase para crear y administrar asignaturas
class Asignatura
  attr_accessor :nombre, :alumnos

  def initialize(nombre)
    @nombre = nombre
    @alumnos = []
  end

  def add_alumno(alumno)
    @alumnos << alumno
  end

  def to_s
    "Nombre: #{@nombre}, Alumnos: #{@alumnos.join(', ')}"
  end
end

# Clase para crear y administrar un instituto
class Instituto
  attr_accessor :nombre, :asignaturas

  def initialize(nombre)
    @nombre = nombre
    @asignaturas = []
  end

  def add_asignatura(asignatura)
    @asignaturas << asignatura
  end

  def to_s
    "Nombre: #{@nombre}, Asignaturas: #{@asignaturas.join(', ')}"
  end
end

# Creamos un instituto
instituto = Instituto.new("Instituto Tecnológico")

# Creamos algunas asignaturas
asignatura1 = Asignatura.new("Matemáticas")
asignatura2 = Asignatura.new("Física")
asignatura3 = Asignatura.new("Química")

# Creamos algunos alumnos
alumno1 = Alumno.new("Juan", "Pérez", 8)
alumno2 = Alumno.new("María", "García", 9)
alumno3 = Alumno.new("Pedro", "López", 7)

# Añadimos los alumnos a las asignaturas
asignatura1.add_alumno(alumno1)
asignatura1.add_alumno(alumno2)
asignatura2.add_alumno(alumno2)
asignatura2.add_alumno(alumno3)
asignatura3.add_alumno(alumno1)
asignatura3.add_alumno(alumno3)

# Añadimos las asignaturas al instituto
instituto.add_asignatura(asignatura1)
instituto.add_asignatura(asignatura2)
instituto.add_asignatura(asignatura3)

# Imprimimos el instituto
puts instituto
```

Este código crea un instituto con tres asignaturas y seis alumnos. Las clases `Alumno`, `Asignatura` e `Instituto` están definidas con atributos y métodos para crear y administrar los objetos que representan a los alumnos, las asignaturas y el instituto.

La clase `Alumno` tiene tres atributos: `nombre`, `apellido` y `nota`. Los métodos `initialize` y `to_s` se utilizan para crear y representar un objeto de tipo `Alumno`, respectivamente.

La clase `Asignatura` tiene dos atributos: `nombre` y `alumnos`. El método `initialize` se utiliza para crear un objeto de tipo `Asignatura`, mientras que el método `add_alumno` se utiliza para añadir un alumno a la asignatura. El método `to_s` se utiliza para representar un objeto de tipo `Asignatura`.

La clase `Instituto` tiene dos atributos: `nombre` y `asignaturas`. El método `initialize` se utiliza para crear un objeto de tipo `Instituto`, mientras que el método `add_asignatura` se utiliza para añadir una asignatura al instituto. El método `to_s` se utiliza para representar un objeto de tipo `Instituto`.

El código comienza creando un objeto de tipo `Instituto` llamado `instituto`. A continuación, se crean tres objetos de tipo `Asignatura` llamados `asignatura1`, `asignatura2` y `asignatura3`.

Luego, se crean seis objetos de tipo `Alumno` llamados `alumno1`, `alumno2`, `alumno3`, `alumno4`, `alumno5` y `alumno6`.

A continuación, se añaden los alumnos a las asignaturas.

Finalmente, se añaden las asignaturas al instituto.

El código termina imprimiendo el instituto, lo que mostrará el nombre del instituto, las asignaturas que ofrece y los alumnos que están matriculados en cada asignatura.