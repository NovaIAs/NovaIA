```ruby
# Este es un código Ruby que crea una clase llamada "Persona" con atributos como el nombre, la edad y el sexo.
# También incluye métodos para obtener y modificar estos atributos.

class Persona
  # El constructor de la clase "Persona" toma tres argumentos: nombre, edad y sexo.
  def initialize(nombre, edad, sexo)
    # Los atributos de la persona se inicializan con los argumentos pasados al constructor.
    @nombre = nombre
    @edad = edad
    @sexo = sexo
  end

  # Este método devuelve el nombre de la persona.
  def nombre
    @nombre
  end

  # Este método devuelve la edad de la persona.
  def edad
    @edad
  end

  # Este método devuelve el sexo de la persona.
  def sexo
    @sexo
  end

  # Este método modifica el nombre de la persona.
  def nombre=(nombre)
    @nombre = nombre
  end

  # Este método modifica la edad de la persona.
  def edad=(edad)
    @edad = edad
  end

  # Este método modifica el sexo de la persona.
  def sexo=(sexo)
    @sexo = sexo
  end

  # Este método devuelve una cadena con información sobre la persona.
  def to_s
    "Nombre: #{@nombre}, Edad: #{@edad}, Sexo: #{@sexo}"
  end
end

# Creamos una nueva instancia de la clase "Persona" llamada "juan".
juan = Persona.new("Juan", 25, "Masculino")

# Imprimimos en pantalla el nombre de "juan".
puts juan.nombre

# Modificamos el nombre de "juan" a "Juan Carlos".
juan.nombre = "Juan Carlos"

# Imprimimos en pantalla el nuevo nombre de "juan".
puts juan.nombre

# Imprimimos en pantalla la edad de "juan".
puts juan.edad

# Modificamos la edad de "juan" a 26.
juan.edad = 26

# Imprimimos en pantalla la nueva edad de "juan".
puts juan.edad

# Imprimimos en pantalla el sexo de "juan".
puts juan.sexo

# Modificamos el sexo de "juan" a "Femenino".
juan.sexo = "Femenino"

# Imprimimos en pantalla el nuevo sexo de "juan".
puts juan.sexo

# Imprimimos en pantalla una cadena con información sobre "juan".
puts juan.to_s
```

**Explicación del código:**

* La primera línea del código crea una clase llamada "Persona".
* Las siguientes líneas definen los atributos de la clase "Persona": nombre, edad y sexo.
* El constructor de la clase "Persona" toma tres argumentos: nombre, edad y sexo.
* Los atributos de la persona se inicializan con los argumentos pasados al constructor.
* Los métodos `nombre`, `edad` y `sexo` devuelven los valores de los atributos correspondientes.
* Los métodos `nombre=`, `edad=` y `sexo=` modifican los valores de los atributos correspondientes.
* El método `to_s` devuelve una cadena con información sobre la persona.
* Creamos una nueva instancia de la clase "Persona" llamada "juan" y le pasamos los valores "Juan", 25 y "Masculino" como argumentos al constructor.
* Imprimimos en pantalla el nombre de "juan".
* Modificamos el nombre de "juan" a "Juan Carlos".
* Imprimimos en pantalla el nuevo nombre de "juan".
* Imprimimos en pantalla la edad de "juan".
* Modificamos la edad de "juan" a 26.
* Imprimimos en pantalla la nueva edad de "juan".
* Imprimimos en pantalla el sexo de "juan".
* Modificamos el sexo de "juan" a "Femenino".
* Imprimimos en pantalla el nuevo sexo de "juan".
* Imprimimos en pantalla una cadena con información sobre "juan".