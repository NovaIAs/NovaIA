```ruby
# Este programa simula un bucle de eventos simple que responde a una entrada de usuario.

# Se define una clase Evento que representa un evento que se puede producir en el sistema.
class Evento
  attr_reader :tipo, :datos

  def initialize(tipo, datos)
    @tipo = tipo
    @datos = datos
  end
end

# Se define una clase GestorEventos que gestiona los eventos que se producen en el sistema.
class GestorEventos
  def initialize(&bloque)
    @bloque = bloque
  end

  def gestionar(evento)
    @bloque.call(evento)
  end
end

# Se define una clase InterfazUsuario que representa la interfaz de usuario del sistema.
class InterfazUsuario
  def initialize(gestor_eventos)
    @gestor_eventos = gestor_eventos
  end

  def esperar_entrada
    # Se espera a que el usuario introduzca una línea de texto.
    entrada = gets.chomp

    # Se crea un evento de tipo "entrada_usuario" con los datos introducidos por el usuario.
    evento = Evento.new("entrada_usuario", entrada)

    # Se envía el evento al gestor de eventos para que lo gestione.
    @gestor_eventos.gestionar(evento)
  end
end

# Se define una clase ProcesadorEventos que procesa los eventos que se producen en el sistema.
class ProcesadorEventos
  def initialize
    @eventos_pendientes = []
  end

  def gestionar(evento)
    # Se añade el evento a la lista de eventos pendientes.
    @eventos_pendientes << evento

    # Se procesan todos los eventos pendientes.
    while @eventos_pendientes.any?
      evento = @eventos_pendientes.shift

      # Se procesa el evento en función de su tipo.
      case evento.tipo
      when "entrada_usuario"
        # Se procesa la entrada del usuario.
        procesar_entrada_usuario(evento.datos)
      else
        # Se ignora el evento.
      end
    end
  end

  def procesar_entrada_usuario(entrada)
    # Se comprueba si el usuario ha introducido "salir".
    if entrada == "salir"
      # Se termina el programa.
      exit
    else
      # Se muestra la entrada introducida por el usuario.
      puts "Entrada: #{entrada}"
    end
  end
end

# Se crea una instancia del gestor de eventos.
gestor_eventos = GestorEventos.new

# Se crea una instancia del procesador de eventos.
procesador_eventos = ProcesadorEventos.new

# Se crea una instancia de la interfaz de usuario.
interfaz_usuario = InterfazUsuario.new(gestor_eventos)

# Se inicia el bucle de eventos.
while true
  # Se espera a que el usuario introduzca una entrada.
  interfaz_usuario.esperar_entrada

  # Se procesan los eventos pendientes.
  procesador_eventos.gestionar(Evento.new("evento_interno"))
end
```

Este código es un ejemplo de un bucle de eventos simple que responde a una entrada de usuario. El código se explica a continuación:

* Se define una clase Evento que representa un evento que se puede producir en el sistema.
* Se define una clase GestorEventos que gestiona los eventos que se producen en el sistema.
* Se define una clase InterfazUsuario que representa la interfaz de usuario del sistema.
* Se define una clase ProcesadorEventos que procesa los eventos que se producen en el sistema.
* Se crea una instancia del gestor de eventos, del procesador de eventos y de la interfaz de usuario.
* Se inicia el bucle de eventos, que espera a que el usuario introduzca una entrada, procesa los eventos pendientes y se repite.

Cuando el usuario introduce una entrada, se crea un evento de tipo "entrada_usuario" con los datos introducidos por el usuario. El evento se envía al gestor de eventos, que lo envía al procesador de eventos para que lo procese. El procesador de eventos comprueba si el usuario ha introducido "salir". Si es así, se termina el programa. De lo contrario, se muestra la entrada introducida por el usuario.