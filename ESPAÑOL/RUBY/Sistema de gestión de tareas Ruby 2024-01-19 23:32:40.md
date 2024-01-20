```ruby
# Requerir bibliotecas
require 'sqlite3'
require 'active_record'
require 'sinatra'
require 'sinatra/flash'
require 'rack-flash'

# Configurar base de datos
ActiveRecord::Base.establish_connection(
  adapter: 'sqlite3',
  database: 'tareas.db'
)

# Crear tabla de tareas
ActiveRecord::Schema.define do
  create_table :tareas do |t|
    t.string :titulo
    t.text :descripcion
    t.boolean :completada, default: false
    t.timestamps
  end
end

# Definir modelo de tarea
class Tarea < ActiveRecord::Base
end

# Configurar sinatra
set :port, 4567
set :public_folder, 'public'

# Crear página de inicio
get '/' do
  @tareas = Tarea.all
  erb :index
end

# Crear nueva tarea
post '/tareas' do
  @tarea = Tarea.new(params[:tarea])
  if @tarea.save
    flash[:notice] = 'Tarea creada con éxito'
    redirect '/'
  else
    flash[:error] = @tarea.errors.full_messages.join(', ')
    erb :new
  end
end

# Mostrar formulario para nueva tarea
get '/tareas/new' do
  @tarea = Tarea.new
  erb :new
end

# Editar tarea
get '/tareas/:id/edit' do
  @tarea = Tarea.find(params[:id])
  erb :edit
end

# Actualizar tarea
put '/tareas/:id' do
  @tarea = Tarea.find(params[:id])
  if @tarea.update(params[:tarea])
    flash[:notice] = 'Tarea actualizada con éxito'
    redirect '/'
  else
    flash[:error] = @tarea.errors.full_messages.join(', ')
    erb :edit
  end
end

# Eliminar tarea
delete '/tareas/:id' do
  @tarea = Tarea.find(params[:id])
  @tarea.destroy
  flash[:notice] = 'Tarea eliminada con éxito'
  redirect '/'
end

# Marcar tarea como completada
post '/tareas/:id/completar' do
  @tarea = Tarea.find(params[:id])
  @tarea.completada = true
  @tarea.save
  redirect '/'
end

# Desmarcar tarea como completada
post '/tareas/:id/descompletar' do
  @tarea = Tarea.find(params[:id])
  @tarea.completada = false
  @tarea.save
  redirect '/'
end

```

Este código crea una aplicación web sencilla para gestionar tareas. La aplicación utiliza la biblioteca Sinatra para crear un servidor web, la biblioteca ActiveRecord para gestionar la base de datos y la biblioteca Rack-Flash para mostrar mensajes flash.

El código crea una tabla de tareas en la base de datos y define un modelo de tarea que representa una fila de la tabla. El código también define rutas para crear, editar, actualizar, eliminar y completar tareas.

La aplicación web tiene una página de inicio que muestra todas las tareas, una página para crear una nueva tarea, una página para editar una tarea existente y una página para completar una tarea. La aplicación también muestra mensajes flash para indicar si una tarea se ha creado, editado, actualizado, eliminado o completado con éxito.