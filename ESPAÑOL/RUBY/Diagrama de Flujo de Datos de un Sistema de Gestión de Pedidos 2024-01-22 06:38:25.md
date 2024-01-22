```ruby
# Una clase para representar un diagrama de flujo de datos (DFD)
class DiagramaDeFlujoDeDatos
    # Constructor de la clase
    def initialize(nombre)
        @nombre = nombre
        @flujos_de_datos = []
        @procesos = []
        @almacenes_de_datos = []
    end

    # Método para agregar un flujo de datos al diagrama
    def agregar_flujo_de_datos(nombre, origen, destino)
        @flujos_de_datos << FlujoDeDatos.new(nombre, origen, destino)
    end

    # Método para agregar un proceso al diagrama
    def agregar_proceso(nombre, entradas, salidas)
        @procesos << Proceso.new(nombre, entradas, salidas)
    end

    # Método para agregar un almacén de datos al diagrama
    def agregar_almacen_de_datos(nombre)
        @almacenes_de_datos << AlmacenDeDatos.new(nombre)
    end

    # Método para mostrar el diagrama en forma de texto
    def mostrar_diagrama
        puts "Nombre del diagrama: #{@nombre}"
        puts "Flujos de datos:"
        @flujos_de_datos.each do |flujo_de_datos|
            puts " - #{flujo_de_datos}"
        end
        puts "Procesos:"
        @procesos.each do |proceso|
            puts " - #{proceso}"
        end
        puts "Almacenes de datos:"
        @almacenes_de_datos.each do |almacen_de_datos|
            puts " - #{almacen_de_datos}"
        end
    end
end

# Una clase para representar un flujo de datos
class FlujoDeDatos
    # Constructor de la clase
    def initialize(nombre, origen, destino)
        @nombre = nombre
        @origen = origen
        @destino = destino
    end

    # Método para obtener el nombre del flujo de datos
    def nombre
        @nombre
    end

    # Método para obtener el origen del flujo de datos
    def origen
        @origen
    end

    # Método para obtener el destino del flujo de datos
    def destino
        @destino
    end

    # Método para mostrar el flujo de datos en forma de texto
    def to_s
        "Flujo de datos: #{@nombre} (de #{@origen} a #{@destino})"
    end
end

# Una clase para representar un proceso
class Proceso
    # Constructor de la clase
    def initialize(nombre, entradas, salidas)
        @nombre = nombre
        @entradas = entradas
        @salidas = salidas
    end

    # Método para obtener el nombre del proceso
    def nombre
        @nombre
    end

    # Método para obtener las entradas del proceso
    def entradas
        @entradas
    end

    # Método para obtener las salidas del proceso
    def salidas
        @salidas
    end

    # Método para mostrar el proceso en forma de texto
    def to_s
        "Proceso: #{@nombre} (entradas: #{@entradas}, salidas: #{@salidas})"
    end
end

# Una clase para representar un almacén de datos
class AlmacenDeDatos
    # Constructor de la clase
    def initialize(nombre)
        @nombre = nombre
    end

    # Método para obtener el nombre del almacén de datos
    def nombre
        @nombre
    end

    # Método para mostrar el almacén de datos en forma de texto
    def to_s
        "Almacén de datos: #{@nombre}"
    end
end

# Crear un diagrama de flujo de datos
diagrama = DiagramaDeFlujoDeDatos.new("Diagrama de flujo de datos de un sistema de gestión de pedidos")

# Agregar flujos de datos al diagrama
diagrama.agregar_flujo_de_datos("Pedido", "Cliente", "Sistema")
diagrama.agregar_flujo_de_datos("Información de producto", "Sistema", "Cliente")
diagrama.agregar_flujo_de_datos("Inventario", "Sistema", "Almacén")
diagrama.agregar_flujo_de_datos("Pedido procesado", "Sistema", "Almacén")
diagrama.agregar_flujo_de_datos("Producto enviado", "Almacén", "Cliente")

# Agregar procesos al diagrama
diagrama.agregar_proceso("Recibir pedido", ["Pedido"], ["Información de producto"])
diagrama.agregar_proceso("Procesar pedido", ["Información de producto", "Inventario"], ["Pedido procesado"])
diagrama.agregar_proceso("Enviar producto", ["Pedido procesado"], ["Producto enviado"])

# Agregar almacenes de datos al diagrama
diagrama.agregar_almacen_de_datos("Base de datos de clientes")
diagrama.agregar_almacen_de_datos("Base de datos de productos")
diagrama.agregar_almacen_de_datos("Almacén")

# Mostrar el diagrama en forma de texto
diagrama.mostrar_diagrama

```

Este código crea un diagrama de flujo de datos para un sistema de gestión de pedidos. El diagrama incluye flujos de datos entre los distintos componentes del sistema, así como los procesos y almacenes de datos que intervienen en el proceso de gestión de pedidos.