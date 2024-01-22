```ruby
# Este código simula una base de datos de usuarios y sus preferencias de películas.

# Creamos una clase Usuario que tendrá los atributos nombre y preferencias.
class Usuario
  attr_accessor :nombre, :preferencias

  # Inicializamos los atributos de la clase Usuario.
  def initialize(nombre, preferencias)
    @nombre = nombre
    @preferencias = preferencias
  end

  # Definimos un método para imprimir el nombre y las preferencias del usuario.
  def imprimir_usuario
    puts "Nombre: #{@nombre}"
    puts "Preferencias: #{@preferencias}"
  end
end

# Creamos una clase Pelicula que tendrá los atributos título, género y año de lanzamiento.
class Pelicula
  attr_accessor :título, :género, :año

  # Inicializamos los atributos de la clase Pelicula.
  def initialize(título, género, año)
    @título = título
    @género = género
    @año = año
  end

  # Definimos un método para imprimir el título, el género y el año de la película.
  def imprimir_pelicula
    puts "Título: #{@título}"
    puts "Género: #{@género}"
    puts "Año: #{@año}"
  end
end

# Creamos una clase Recomendador que tendrá el método recomendar_peliculas.
class Recomendador
  # Definimos el método recomendar_peliculas que recibe como parámetros un usuario y una lista de películas.
  def recomendar_peliculas(usuario, películas)
    # Obtenemos las preferencias del usuario.
    preferencias = usuario.preferencias

    # Creamos un array para almacenar las películas recomendadas.
    recomendaciones = []

    # Recorremos la lista de películas.
    películas.each do |película|
      # Obtenemos el género de la película.
      género = película.género

      # Comprobamos si el género de la película está en las preferencias del usuario.
      if preferencias.include? género
        # Si el género de la película está en las preferencias del usuario, añadimos la película al array de recomendaciones.
        recomendaciones << película
      end
    end

    # Devolvemos el array de recomendaciones.
    recomendaciones
  end
end

# Creamos un array de usuarios.
usuarios = [
  Usuario.new("Juan", ["Acción", "Comedia", "Terror"]),
  Usuario.new("María", ["Romance", "Drama", "Musical"]),
  Usuario.new("Pedro", ["Ciencia ficción", "Fantasía", "Animación"])
]

# Creamos un array de películas.
películas = [
  Pelicula.new("El padrino", "Drama", 1972),
  Pelicula.new("El caballero oscuro", "Acción", 2008),
  Pelicula.new("El rey león", "Animación", 1994),
  Pelicula.new("Titanic", "Romance", 1997),
  Pelicula.new("La La Land", "Musical", 2016),
  Pelicula.new("Star Wars", "Ciencia ficción", 1977),
  Pelicula.new("El señor de los anillos", "Fantasía", 2001),
  Pelicula.new("Psicosis", "Terror", 1960),
  Pelicula.new("Pulp Fiction", "Comedia", 1994),
  Pelicula.new("El club de la lucha", "Drama", 1999)
]

# Creamos un objeto Recomendador.
recomendador = Recomendador.new

# Recorremos los usuarios.
usuarios.each do |usuario|
  # Obtenemos las películas recomendadas para el usuario.
  recomendaciones = recomendador.recomendar_peliculas(usuario, películas)

  # Imprimimos el nombre del usuario.
  puts "Nombre: #{usuario.nombre}"

  # Imprimimos las películas recomendadas para el usuario.
  puts "Recomendaciones:"
  recomendaciones.each do |película|
    puts película.título
  end

  # Imprimimos una línea en blanco.
  puts
end
```

Este código utiliza clases y objetos para simular una base de datos de usuarios y sus preferencias de películas. También utiliza un método para recomendar películas a los usuarios en función de sus preferencias.

El código primero crea un array de usuarios y un array de películas. A continuación, crea un objeto Recomendador. Luego, recorre los usuarios y obtiene las películas recomendadas para cada usuario utilizando el método recomendar_peliculas.

Finalmente, imprime el nombre del usuario y las películas recomendadas para él.