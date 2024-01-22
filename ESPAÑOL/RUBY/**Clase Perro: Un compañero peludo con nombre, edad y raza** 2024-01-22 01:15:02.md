```ruby
# Definir una clase llamada Perro
class Perro
    # Crear un atributo para el nombre del perro
    attr_accessor :nombre

    #Crear un atributo para la edad del perro
    attr_accessor :edad

    #Crear un atributo para la raza del perro
    attr_accessor :raza

    # Crear un método para inicializar el perro
    def initialize(nombre, edad, raza)
        @nombre = nombre
        @edad = edad
        @raza = raza
    end

    # Crear un método para ladrar
    def ladrar
        puts "Guau guau!"
    end

    # Crear un método para mostrar la información del perro
    def mostrar_informacion
        puts "Nombre: #{@nombre}"
        puts "Edad: #{@edad}"
        puts "Raza: #{@raza}"
    end
end

# Crear una instancia de la clase Perro
perro1 = Perro.new("Firulais", 5, "Pastor alemán")

# Mostrar la información del perro
perro1.mostrar_informacion

# Hacer que el perro ladre
perro1.ladrar

# Crear un array de perros
perros = [
    perro1,
    Perro.new("Toby", 3, "Golden Retriever"),
    Perro.new("Luna", 2, "Chihuahua")
]

# Iterar sobre el array de perros y mostrar su información
perros.each do |perro|
    perro.mostrar_informacion
end
```

Explicación del código:

* Se define una clase llamada `Perro` con tres atributos: `nombre`, `edad` y `raza`.


* Se crea un método `initialize` que se llama cuando se crea una nueva instancia de la clase `Perro`. Este método toma tres argumentos: `nombre`, `edad` y `raza`, y los asigna a los atributos correspondientes.


* Se crea un método llamado `ladrar` que imprime "Guau guau!" en la consola.


* Se crea un método llamado `mostrar_informacion` que imprime el nombre, la edad y la raza del perro en la consola.


* Se crea una instancia de la clase `Perro` llamada `perro1` y se inicializa con los valores "Firulais", 5 y "Pastor alemán".


* Se llama al método `mostrar_informacion` de `perro1` para mostrar su información en la consola.


* Se llama al método `ladrar` de `perro1` para hacer que ladre.


* Se crea un array llamado `perros` con tres elementos: `perro1`, `perro2` y `perro3`.


* Se itera sobre el array `perros` y se llama al método `mostrar_informacion` de cada perro para mostrar su información en la consola.