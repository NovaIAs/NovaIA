```ruby
# Este código crea una lista de estudiantes y sus calificaciones, luego muestra la lista
# y el promedio de las calificaciones.

# Se crea una lista de estudiantes y sus calificaciones.
estudiantes = [
  { nombre: "Juan", calificaciones: [8, 9, 7] },
  { nombre: "María", calificaciones: [9, 10, 8] },
  { nombre: "Pedro", calificaciones: [7, 6, 9] }
]

# Se muestra la lista de estudiantes y sus calificaciones.
puts "Lista de estudiantes y sus calificaciones:"
estudiantes.each do |estudiante|
  puts "#{estudiante[:nombre]}: #{estudiante[:calificaciones].join(', ')}"
end

# Se calcula el promedio de las calificaciones de cada estudiante.
promedios = estudiantes.map do |estudiante|
  estudiante[:promedio] = estudiante[:calificaciones].sum / estudiante[:calificaciones].size
end

# Se muestra el promedio de las calificaciones de cada estudiante.
puts "Promedio de las calificaciones:"
estudiantes.each do |estudiante|
  puts "#{estudiante[:nombre]}: #{estudiante[:promedio]}"
end

# Se calcula el promedio general de las calificaciones.
promedio_general = estudiantes.map { |estudiante| estudiante[:promedio] }.sum / estudiantes.size

# Se muestra el promedio general de las calificaciones.
puts "Promedio general: #{promedio_general}"
```

Explicación del código:

* Se crea una lista de estudiantes y sus calificaciones utilizando un array de hashes. Cada hash contiene el nombre del estudiante y una lista de sus calificaciones.
* Se utiliza el método `each` para mostrar la lista de estudiantes y sus calificaciones.
* Se utiliza el método `map` para calcular el promedio de las calificaciones de cada estudiante.
* Se utiliza el método `each` para mostrar el promedio de las calificaciones de cada estudiante.
* Se utiliza el método `map` para calcular el promedio general de las calificaciones.
* Se utiliza el método `sum` para sumar las calificaciones de todos los estudiantes.
* Se utiliza el método `size` para obtener el número de estudiantes.
* Se utiliza el método `puts` para mostrar el promedio general de las calificaciones.