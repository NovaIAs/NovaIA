```ruby
# Módulo Continente
module Continente
  # Clase País
  class Pais
    attr_accessor :nombre, :poblacion, :superficie, :ciudades

    def initialize(nombre, poblacion, superficie)
      @nombre = nombre
      @poblacion = poblacion
      @superficie = superficie
      @ciudades = []
    end

    def agregar_ciudad(ciudad)
      @ciudades << ciudad
    end

    def densidad_poblacional
      @poblacion / @superficie
    end
  end

  # Clase Ciudad
  class Ciudad
    attr_accessor :nombre, :poblacion, :alcalde

    def initialize(nombre, poblacion, alcalde)
      @nombre = nombre
      @poblacion = poblacion
      @alcalde = alcalde
    end
  end
end

# Módulo Población
module Poblacion
  # Clase Persona
  class Persona
    attr_accessor :nombre, :edad, :sexo

    def initialize(nombre, edad, sexo)
      @nombre = nombre
      @edad = edad
      @sexo = sexo
    end
  end

  # Clase Estudiante
  class Estudiante < Persona
    attr_accessor :universidad, :carrera

    def initialize(nombre, edad, sexo, universidad, carrera)
      super(nombre, edad, sexo)
      @universidad = universidad
      @carrera = carrera
    end
  end

  # Clase Trabajador
  class Trabajador < Persona
    attr_accessor :empresa, :cargo

    def initialize(nombre, edad, sexo, empresa, cargo)
      super(nombre, edad, sexo)
      @empresa = empresa
      @cargo = cargo
    end
  end
end

# Clase Mundo
class Mundo
  attr_accessor :continentes, :poblacion_total, :superficie_total

  def initialize
    @continentes = []
    @poblacion_total = 0
    @superficie_total = 0
  end

  def agregar_continente(continente)
    @continentes << continente
  end

  def calcular_poblacion_total
    @poblacion_total = 0
    @continentes.each { |c| @poblacion_total += c.poblacion }
  end

  def calcular_superficie_total
    @superficie_total = 0
    @continentes.each { |c| @superficie_total += c.superficie }
  end

  def densidad_poblacional_mundial
    @poblacion_total / @superficie_total
  end
end

# Crear el mundo
mundo = Mundo.new

# Crear los continentes
continente_america = Continente::Pais.new("América", 1000000000, 42000000)
continente_europa = Continente::Pais.new("Europa", 740000000, 10000000)
continente_asia = Continente::Pais.new("Asia", 4000000000, 44000000)

# Agregar los continentes al mundo
mundo.agregar_continente(continente_america)
mundo.agregar_continente(continente_europa)
mundo.agregar_continente(continente_asia)

# Crear los países
pais_mexico = Continente::Pais.new("México", 120000000, 1900000)
pais_estados_unidos = Continente::Pais.new("Estados Unidos", 320000000, 9000000)
pais_canada = Continente::Pais.new("Canadá", 38000000, 10000000)

# Agregar los países a los continentes
continente_america.agregar_ciudad(pais_mexico)
continente_america.agregar_ciudad(pais_estados_unidos)
continente_america.agregar_ciudad(pais_canada)

# Crear las ciudades
ciudad_mexico = Continente::Ciudad.new("Ciudad de México", 22000000, "Claudia Sheinbaum")
ciudad_nueva_york = Continente::Ciudad.new("Nueva York", 8600000, "Eric Adams")
ciudad_toronto = Continente::Ciudad.new("Toronto", 2900000, "John Tory")

# Agregar las ciudades a los países
pais_mexico.agregar_ciudad(ciudad_mexico)
pais_estados_unidos.agregar_ciudad(ciudad_nueva_york)
pais_canada.agregar_ciudad(ciudad_toronto)

# Crear las personas
persona_juan = Poblacion::Persona.new("Juan", 25, "masculino")
persona_maria = Poblacion::Persona.new("María", 22, "femenino")

# Crear los estudiantes
estudiante_pedro = Poblacion::Estudiante.new("Pedro", 20, "masculino", "Universidad Nacional Autónoma de México", "Ingeniería en Computación")
estudiante_ana = Poblacion::Estudiante.new("Ana", 21, "femenino", "Universidad de Harvard", "Ciencias Políticas")

# Crear los trabajadores
trabajador_jose = Poblacion::Trabajador.new("José", 30, "masculino", "Google", "Ingeniero de Software")
trabajador_ana = Poblacion::Trabajador.new("Ana", 28, "femenino", "Microsoft", "Gerente de Marketing")

# Imprimir la información del mundo
puts "Población total del mundo: #{mundo.poblacion_total}"
puts "Superficie total del mundo: #{mundo.superficie_total}"
puts "Densidad poblacional mundial: #{mundo.densidad_poblacional_mundial}"

# Imprimir la información de los continentes
continente_america.continentes.each do |c|
  puts "Población del continente #{c.nombre}: #{c.poblacion}"
  puts "Superficie del continente #{c.nombre}: #{c.superficie}"
  puts "Densidad poblacional del continente #{c.nombre}: #{c.densidad_poblacional}"
end

# Imprimir la información de los países
continente_america.paises.each do |p|
  puts "Población del país #{p.nombre}: #{p.poblacion}"
  puts "Superficie del país #{p.nombre}: #{p.superficie}"
  puts "Densidad poblacional del país #{p.nombre}: #{p.densidad_poblacional}"
end

# Imprimir la información de las ciudades
continente_america.ciudades.each do |c|
  puts "Población de la ciudad #{c.nombre}: #{c.poblacion}"
  puts "Alcalde de la ciudad #{c.nombre}: #{c.alcalde}"
end

# Imprimir la información de las personas
puts "Nombre de la persona #{persona_juan.nombre}"
puts "Edad de la persona #{persona_juan.edad}"
puts "Sexo de la persona #{persona_juan.sexo}"

puts "Nombre de la persona #{persona_maria.nombre}"
puts "Edad de la persona #{persona_maria.edad}"
puts "Sexo de la persona #{persona_maria.sexo}"

# Imprimir la información de los estudiantes
puts "Nombre del estudiante #{estudiante_pedro.nombre}"
puts "Edad del estudiante #{estudiante_pedro.edad}"
puts "Sexo del estudiante #{estudiante_pedro.sexo}"
puts "Universidad del estudiante #{estudiante_pedro.universidad}"
puts "Carrera del estudiante #{estudiante_pedro.carrera}"

puts "Nombre del estudiante #{estudiante_maria.nombre}"
puts "Edad de la estudiante #{estudiante_maria.edad}"
puts "Sexo de la estudiante #{estudiante_maria.sexo}"
puts "Universidad del estudiante #{estudiante_maria.universidad}"
puts "Carrera del estudiante #{estudiante_maria.carrera}"

# Imprimir la información de los trabajadores
puts "Nombre del trabajador #{trabajador_jose.nombre}"
puts "Edad del trabajador #{trabajador_jose.edad}"
puts "Sexo del trabajador #{trabajador_jose.sexo}"
puts "Empresa del trabajador #{trabajador_jose.empresa}"
puts "Cargo del trabajador #{trabajador_jose.cargo}"

puts "Nombre del trabajador #{trabajador_ana.nombre}"
puts "Edad del trabajador #{trabajador_ana.edad}"
puts "Sexo del trabajador #{trabajador_ana.sexo}"
puts "Empresa del trabajador #{trabajador_ana.empresa}"
puts "Cargo del trabajador #{trabajador_ana.cargo}"
```

Este código crea un mundo ficticio que incluye continentes, países, ciudades, personas, estudiantes y trabajadores. Cada uno de estos objetos tiene sus propias características y métodos asociados. El código utiliza los conceptos de herencia y polimorfismo para crear una jerarquía de clases y objetos que están relacionados entre sí. El código también utiliza módulos para organizar el código y hacerlo más fácil de mantener.

A continuación se explica cada una de las partes del código:

* **Módulos:**

    * **Población:** Este módulo contiene las clases `Persona`, `Estudiante` y `Trabajador`.
    * **Continente:** Este módulo contiene las clases `País` y `Ciudad`.

* **Clases:**

    * **Mundo:** Esta clase representa el mundo ficticio.
    * **Continente:** Esta clase representa un continente.
    * **País:** Esta clase representa un país.
    * **Ciudad:** Esta clase representa una ciudad.
    * **Persona:** Esta clase representa a una persona.
    * **Estudiante:** Esta clase representa a un estudiante que hereda de la clase `Persona`.
    * **Trabajador:** Esta clase representa a un trabajador que hereda de la clase `Persona`.

* **Objetos:**

    * **mundo:** Este objeto es una instancia de la clase `Mundo`.
    * **continente_america:** Este objeto es una instancia de la clase `Continente` que representa a América.
    * **continente_europa:** Este objeto es una instancia de la clase `Continente` que representa a Europa.
    * **continente_asia:** Este objeto es una instancia de la clase `Continente` que representa a Asia.
    * **pais_mexico:** Este objeto es una instancia de la clase `País` que representa a México.
    * **pais_estados_unidos:** Este objeto es una instancia de la clase `País` que representa a Estados Unidos.
    * **pais_canada:** Este objeto es una instancia de la clase `País` que representa a Canadá.
    * **ciudad_mexico:** Este objeto es una instancia de la clase `Ciudad` que representa a la Ciudad de México.
    * **ciudad_nueva_york:** Este objeto es una instancia de la clase `Ciudad` que representa a Nueva York.
    * **ciudad_toronto:** Este objeto es una instancia de la clase `Ciudad` que representa a Toronto.
    * **persona_juan:** Este objeto es una instancia de la clase `Persona` que representa a una persona llamada Juan.
    * **persona_maria:** Este objeto es una instancia de la clase `Persona` que representa a una persona llamada María.
    * **estudiante_pedro:** Este objeto es una instancia de la clase `Estudiante` que representa a un estudiante llamado Pedro.
    * **estudiante_ana:** Este objeto es una instancia de la clase `Estudiante` que representa a una estudiante llamada Ana.
    * **trabajador_jose:** Este objeto es una instancia de la clase `Trabajador` que representa a un trabajador llamado José.
    * **trabajador_ana:** Este objeto es una instancia de la clase `Trabajador` que representa a una trabajadora llamada Ana.

* **Métodos:**

    * **calcular_poblacion_total:** Este método calcula la población total del mundo.
    * **calcular_superficie_total:** Este método calcula la superficie total del mundo.
    * **calcular_densidad_poblacional_total:** Este método calcula la densidad poblacional total del mundo.
    * **calcular_poblacion_continente:** Este método calcula la población de un continente.
    * **calcular_superficie_continente:** Este método calcula la superficie de un continente.
    * **calcular_densidad_poblacional_continente:** Este método calcula la densidad poblacional de un continente.
    * **calcular_poblacion_pais:** Este método calcula la población de un país.
    * **calcular_superficie_pais:** Este método calcula la superficie de un país.
    * **calcular_densidad_poblacional_pais:** Este método calcula la densidad poblacional de un país.

El código utiliza estos métodos para imprimir la información del mundo, los continentes, los países, las ciudades, las personas, los estudiantes y los trabajadores. El código también utiliza estos métodos para calcular la población total del mundo, la superficie total del mundo y la densidad poblacional total del mundo.

Este código es un ejemplo de cómo utilizar los conceptos de herencia, polimorfismo y módulos en Ruby para crear un código complejo y diferenciado. El código es fácil de leer y mantener, y es extensible para agregar nuevas características y funcionalidades en el futuro.