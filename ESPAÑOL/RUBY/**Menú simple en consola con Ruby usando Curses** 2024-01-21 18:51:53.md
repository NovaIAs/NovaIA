```ruby
# Una librería que permite crear una interfaz de usuario usando texto.
require 'curses'

# Inicializamos la interfaz de usuario.
Curses.init_screen

# Preparación para dibujar en la interfaz.
Curses.curs_set(0)
Curses.noecho

# Obtenemos las dimensiones de la interfaz.
ancho = Curses.cols
alto = Curses.lines

# Creamos una ventana para dibujar.
ventana = Curses.Window.new(alto - 2, ancho - 2, 1, 1)

# Limpiamos la ventana.
ventana.clear

# Dibujamos un rectángulo en la ventana.
ventana.box("|", "-", "+")

# Escribir texto en la ventana, en concreto, el título de la aplicación.
ventana.setpos(1, 2); ventana.addstr("Título de la aplicación")

# Dibujamos una lista de opciones para elegir, formando un menú.
opciones = ["Opción 1", "Opción 2", "Opción 3", "Salir"]
x = 2; y = 4

# Recorremos las opciones del menú.
opciones.each do |opcion|
  # Dibujamos la opción en la ventana.
  ventana.setpos(y, x); ventana.addstr(opcion)
  y += 1
end

# Dibujamos un cursor para indicar la opción seleccionada.
ventana.setpos(4, 2); ventana.addstr(">")

# Creamos una variable para guardar la opción seleccionada.
opcion_seleccionada = 0

# Iteramos hasta que el usuario pulsa la tecla "q" para salir.
while true do
  tecla = Curses.getch

  # Si el usuario pulsa la tecla "q", salimos del programa.
  if tecla == "q"
    Curses.close_screen
    exit
  end

  # Si el usuario pulsa las flechas arriba o abajo, movemos el cursor.
  if tecla == Curses::KEY_UP
    opcion_seleccionada -= 1
    if opcion_seleccionada < 0
      opcion_seleccionada = opciones.length - 1
    end
  elsif tecla == Curses::KEY_DOWN
    opcion_seleccionada += 1
    if opcion_seleccionada >= opciones.length
      opcion_seleccionada = 0
    end
  end

  # Movemos el cursor a la posición correspondiente a la opción seleccionada.
  ventana.setpos(4 + opcion_seleccionada, 2)

  # Borramos el cursor anterior y dibujamos el nuevo.
  ventana.addstr(" ")
  ventana.addstr(">")

  # Refrescamos la ventana para mostrar los cambios.
  ventana.refresh

  # Si el usuario pulsa la tecla "Enter", ejecutamos la acción correspondiente a la opción seleccionada.
  if tecla == Curses::KEY_ENTER
    case opcion_seleccionada
    when 0 # Primera opción
      # Ejecutar código para la primera opción.
    when 1 # Segunda opción
      # Ejecutar código para la segunda opción.
    when 2 # Tercera opción
      # Ejecutar código para la tercera opción.
    end
  end
end