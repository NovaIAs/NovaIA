```ruby
# Carga las gemas necesarias
require 'google/cloud/storage'

# Crea un cliente de Almacenamiento de Google Cloud
storage = Google::Cloud::Storage.new

# Especifica el nombre del bucket
bucket_name = "mi-bucket"

# Crea un nuevo bucket
bucket = storage.create_bucket bucket_name

# Especifica el nombre del archivo
file_name = "archivo.txt"

# Sube un archivo al bucket
file = bucket.create_file file_name, "Contenido del archivo"

# Especifica el nombre del objeto
object_name = "objeto.txt"

# Copia un archivo a un nuevo objeto
bucket.copy_file file_name, object_name

# Elimina el archivo original
file.delete

# Obtiene una lista de los objetos en el bucket
objects = bucket.files

# Elimina todos los objetos del bucket
objects.each do |object|
  object.delete
end

# Elimina el bucket
bucket.delete

# Crea una nueva tabla en BigQuery
bigquery = Google::Cloud::Bigquery.new
dataset = bigquery.dataset "mi_dataset"
table = dataset.create_table "mi_tabla" do |t|
  t.schema do |s|
    s.integer "id", mode: :required
    s.string "nombre", mode: :required
  end
end

# Carga datos en la tabla
data = [
  { id: 1, nombre: "Juan" },
  { id: 2, nombre: "María" }
]
table.insert data

# Ejecuta una consulta en la tabla
results = table.query "SELECT * FROM mi_tabla"

# Imprime los resultados de la consulta
results.each do |row|
  puts "#{row[:id]} #{row[:nombre]}"
end

# Crea una nueva función en Cloud Functions
functions = Google::Cloud::Functions::Client.new
function = functions.create_function "mi_funcion" do |f|
  f.runtime = "nodejs16"
  f.source_code = "exports.helloHttp = (req, res) => { res.send('Hello World!') };"
end

# Invoca la función
response = functions.call "mi_funcion"

# Imprime el resultado de la invocación
puts response.body
```

Este código es un ejemplo complejo de cómo utilizar varias API de Google Cloud en una sola aplicación Ruby. El código crea un bucket de Almacenamiento de Google Cloud, sube un archivo al bucket, copia el archivo a un nuevo objeto, elimina el archivo original, elimina todos los objetos del bucket, elimina el bucket, crea una nueva tabla en BigQuery, carga datos en la tabla, ejecuta una consulta en la tabla, imprime los resultados de la consulta, crea una nueva función en Cloud Functions, invoca la función e imprime el resultado de la invocación.

Este código es bastante complejo y difícil de explicar en detalle en una sola respuesta. Sin embargo, puedes encontrar más información sobre las API de Google Cloud en la documentación oficial:

* Almacenamiento de Google Cloud: https://cloud.google.com/storage/docs/
* BigQuery: https://cloud.google.com/bigquery/docs/
* Cloud Functions: https://cloud.google.com/functions/docs/