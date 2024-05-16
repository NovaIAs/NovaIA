```ruby
# Clase para representar una persona
class Persona
  # Constructor de la clase
  def initialize(nombre, apellidos)
    @nombre = nombre
    @apellidos = apellidos
  end

  # Método para obtener el nombre completo de la persona
  def nombre_completo
    "#{@nombre} #{@apellidos}"
  end
end

# Clase para representar una dirección
class Direccion
  # Constructor de la clase
  def initialize(calle, numero, colonia, municipio, estado, codigo_postal)
    @calle = calle
    @numero = numero
    @colonia = colonia
    @municipio = municipio
    @estado = estado
    @codigo_postal = codigo_postal
  end

  # Método para obtener la dirección completa
  def direccion_completa
    "#{@calle} #{@numero}, #{@colonia}, #{@municipio}, #{@estado}, #{@codigo_postal}"
  end
end

# Clase para representar un contacto
class Contacto
  # Constructor de la clase
  def initialize(nombre, apellidos, direccion, telefono, email)
    @persona = Persona.new(nombre, apellidos)
    @direccion = Direccion.new(*direccion) # Desempaqueta los argumentos de la dirección
    @telefono = telefono
    @email = email
  end

  # Método para obtener toda la información del contacto
  def informacion_completa
    "Nombre: #{@persona.nombre_completo}\n" +
    "Dirección: #{@direccion.direccion_completa}\n" +
    "Teléfono: #{@telefono}\n" +
    "Email: #{@email}"
  end
end

# Crea un contacto
contacto = Contacto.new("Juan", "Pérez", ["Calle Principal", 123, "Centro", "Ciudad Juárez", "Chihuahua", "32100"], "6561234567", "juan.perez@ejemplo.com")

# Imprime la información del contacto
puts contacto.informacion_completa
```

**Explicación del código:**

Este código define tres clases: `Persona`, `Direccion` y `Contacto`.

La clase `Persona` representa una persona con un nombre y apellidos.

La clase `Direccion` representa una dirección con calle, número, colonia, municipio, estado y código postal.

La clase `Contacto` representa un contacto con un nombre, apellidos, dirección, teléfono y correo electrónico.

El constructor de la clase `Contacto` recibe como argumentos un arreglo con los datos de la dirección y los desempaqueta para crear un objeto `Direccion`.

El método `informacion_completa` de la clase `Contacto` regresa toda la información del contacto en un string.

Al final del código se crea un objeto `Contacto` y se imprime su información completa.