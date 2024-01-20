```ruby
# Este código implementa un sistema de gestión de tareas en RUBY.
# Se utiliza una base de datos SQLite para almacenar las tareas.
# El código está dividido en varios módulos para facilitar su mantenimiento.

# Módulo de conexión a la base de datos
module Database
  # Conecta a la base de datos SQLite
  def self.connect
    @db ||= SQLite3::Database.new "tareas.db"
  end

  # Ejecuta una consulta SQL sobre la base de datos
  def self.execute(sql, *args)
    connect.execute(sql, *args)
  end
end

# Módulo de gestión de tareas
module Tasks
  # Crea una nueva tarea
  def self.create(nombre, descripcion, fecha_vencimiento)
    Database.execute("INSERT INTO tareas (nombre, descripcion, fecha_vencimiento) VALUES (?, ?, ?)",
                     nombre, descripcion, fecha_vencimiento)
  end

  # Obtiene todas las tareas
  def self.all
    Database.execute("SELECT * FROM tareas")
  end

  # Obtiene una tarea por su id
  def self.find(id)
    Database.execute("SELECT * FROM tareas WHERE id = ?", id).first
  end

  # Actualiza una tarea
  def self.update(id, nombre, descripcion, fecha_vencimiento)
    Database.execute("UPDATE tareas SET nombre = ?, descripcion = ?, fecha_vencimiento = ? WHERE id = ?",
                     nombre, descripcion, fecha_vencimiento, id)
  end

  # Elimina una tarea
  def self.delete(id)
    Database.execute("DELETE FROM tareas WHERE id = ?", id)
  end
end

# Módulo de interfaz de usuario
module UI
  # Muestra un mensaje al usuario
  def self.puts(message)
    puts message
  end

  # Obtiene un valor del usuario
  def self.gets
    gets.chomp
  end
end

# Módulo principal
module Main
  # Muestra el menú principal
  def self.show_menu
    UI.puts "1. Crear tarea"
    UI.puts "2. Ver todas las tareas"
    UI.puts "3. Buscar tarea por id"
    UI.puts "4. Actualizar tarea"
    UI.puts "5. Eliminar tarea"
    UI.puts "6. Salir"
  end

  # Obtiene la opción del usuario
  def self.get_option
    UI.gets.to_i
  end

  # Procesa la opción elegida por el usuario
  def self.process_option(option)
    case option
    when 1
      create_task
    when 2
      show_all_tasks
    when 3
      find_task_by_id
    when 4
      update_task
    when 5
      delete_task
    when 6
      exit
    else
      UI.puts "Opción no válida"
    end
  end

  # Crea una nueva tarea
  def self.create_task
    UI.puts "Nombre de la tarea:"
    nombre = UI.gets
    UI.puts "Descripción de la tarea:"
    descripcion = UI.gets
    UI.puts "Fecha de vencimiento de la tarea:"
    fecha_vencimiento = UI.gets
    Tasks.create(nombre, descripcion, fecha_vencimiento)
    UI.puts "Tarea creada con éxito"
  end

  # Muestra todas las tareas
  def self.show_all_tasks
    tareas = Tasks.all
    if tareas.empty?
      UI.puts "No hay tareas para mostrar"
    else
      UI.puts "Lista de tareas:"
      tareas.each do |tarea|
        UI.puts "ID: #{tarea["id"]}"
        UI.puts "Nombre: #{tarea["nombre"]}"
        UI.puts "Descripción: #{tarea["descripcion"]}"
        UI.puts "Fecha de vencimiento: #{tarea["fecha_vencimiento"]}"
        UI.puts ""
      end
    end
  end

  # Busca una tarea por su id
  def self.find_task_by_id
    UI.puts "ID de la tarea:"
    id = UI.gets.to_i
    tarea = Tasks.find(id)
    if tarea
      UI.puts "Tarea encontrada:"
      UI.puts "ID: #{tarea["id"]}"
      UI.puts "Nombre: #{tarea["nombre"]}"
      UI.puts "Descripción: #{tarea["descripcion"]}"
      UI.puts "Fecha de vencimiento: #{tarea["fecha_vencimiento"]}"
    else
      UI.puts "Tarea no encontrada"