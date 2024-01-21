```ruby
# Definir una clase llamada "Persona" con atributos "nombre", "edad" y "ocupación".
class Persona
  attr_accessor :nombre, :edad, :ocupacion

  # Método para saludar a una persona.
  def saludar
    "Hola, mi nombre es #{nombre} y tengo #{edad} años. Soy #{ocupacion}."
  end
end

# Crear tres objetos de la clase "Persona" con diferentes valores de atributos.
persona1 = Persona.new
persona1.nombre = "Juan"
persona1.edad = 25
persona1.ocupacion = "ingeniero"

persona2 = Persona.new
persona2.nombre = "María"
persona2.edad = 30
persona2.ocupacion = "doctora"

persona3 = Persona.new
persona3.nombre = "Pedro"
persona3.edad = 35
persona3.ocupacion = "profesor"

# Crear un array con las tres personas.
personas = [persona1, persona2, persona3]

# Iterar sobre el array de personas y saludar a cada una de ellas.
personas.each do |persona|
  puts persona.saludar
end
```

El código anterior crea una clase llamada "Persona" con tres atributos: "nombre", "edad" y "ocupación". También define un método llamado "saludar" que devuelve un saludo personalizado para cada persona.

A continuación, se crean tres objetos de la clase "Persona" con diferentes valores de atributos y se agregan a un array. Finalmente, se itera sobre el array de personas y se llama al método "saludar" para cada persona.

La salida del código será:

```
Hola, mi nombre es Juan y tengo 25 años. Soy ingeniero.
Hola, mi nombre es María y tengo 30 años. Soy doctora.
Hola, mi nombre es Pedro y tengo 35 años. Soy profesor.
```