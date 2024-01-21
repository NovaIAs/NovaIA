```ruby
# Importar las gemas necesarias
require 'nokogiri'
require 'open-uri'
require 'csv'

# Obtener el HTML de la página web
url = 'https://www.ejemplo.com/productos'
html = open(url).read

# Crear un documento Nokogiri a partir del HTML
doc = Nokogiri::HTML(html)

# Crear un array donde almacenar los datos
data = []

# Iterar sobre los elementos de la página que contienen los datos que queremos extraer
doc.css('.producto').each do |producto|
  # Extraer los datos del producto
  nombre = producto.css('.nombre').text
  precio = producto.css('.precio').text
  descripcion = producto.css('.descripcion').text

  # Añadir los datos del producto al array
  data << [nombre, precio, descripcion]
end

# Crear un archivo CSV con los datos extraídos
CSV.open('productos.csv', 'w') do |csv|
  csv << ['Nombre', 'Precio', 'Descripción']
  data.each do |producto|
    csv << producto
  end
end

# Mostrar un mensaje de éxito
puts 'Datos extraídos correctamente y guardados en el archivo "productos.csv"'
```

Este código consta de las siguientes partes:

* **Importación de las gemas necesarias:** Se importan las gemas `nokogiri`, `open-uri` y `csv` que son necesarias para extraer los datos de la página web y guardarlos en un archivo CSV.
* **Obtención del HTML de la página web:** Se utiliza la gema `open-uri` para obtener el código HTML de la página web que contiene los datos que queremos extraer.
* **Creación de un documento Nokogiri a partir del HTML:** El código HTML se convierte en un documento Nokogiri que permite acceder a los elementos de la página web de forma estructurada.
* **Creación de un array para almacenar los datos:** Se crea un array vacío donde se almacenarán los datos extraídos de la página web.
* **Iteración sobre los elementos de la página que contienen los datos:** Se itera sobre los elementos de la página web que contienen los datos que queremos extraer.
* **Extracción de los datos del producto:** Para cada elemento de la página web, se extraen los datos del producto, como el nombre, el precio y la descripción.
* **Adición de los datos del producto al array:** Los datos del producto se añaden al array creado anteriormente.
* **Creación de un archivo CSV con los datos extraídos:** Se utiliza la gema `csv` para crear un archivo CSV con los datos extraídos.
* **Visualización de un mensaje de éxito:** Se muestra un mensaje en la consola indicando que los datos se han extraído correctamente y se han guardado en el archivo CSV.