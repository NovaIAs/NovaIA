```tcl
# Definir una lista de frutas
frutas = [manzana, pera, plátano, naranja, fresa]

# Crear un procedimiento para mostrar cada elemento de la lista
proc mostrar_frutas {lista} {
  foreach fruta $lista {
    puts "Fruta: $fruta"
  }
}

# Llamar al procedimiento para mostrar la lista de frutas
mostrar_frutas $frutas

# Definir una función para calcular el área de un círculo
proc area_circulo {radio} {
  return [expr {pi * $radio * $radio}]
}

# Llamar a la función para calcular el área de un círculo con radio 5
puts "Área del círculo: [area_circulo 5]"

# Crear una clase para representar a un estudiante
class Estudiante {
  constructor {nombre, edad, notas} {
    set self nombre $nombre
    set self edad $edad
    set self notas $notas
  }

  method promedio {self} {
    return [expr {sum($self notas) / llength($self notas)}]
  }
}

# Crear un objeto de la clase Estudiante
estudiante1 = [Estudiante "Juan Pérez" 20 [90 85 95]]

# Obtener el promedio del estudiante
puts "Promedio del estudiante: [estudiante1 promedio]"

# Definir un procedimiento para crear un diccionario
proc crear_diccionario {claves valores} {
  set diccionario {}
  foreach clave $claves {
    foreach valor $valores {
      dict set diccionario $clave $valor
    }
  }
  return $diccionario
}

# Crear un diccionario con claves y valores
diccionario = [crear_diccionario {"clave1" "valor1"} {"clave2" "valor2"}]

# Obtener el valor asociado a la clave "clave1"
puts "Valor asociado a la clave \"clave1\": [dict get $diccionario clave1]"

# Definir un procedimiento para buscar un elemento en una lista
proc buscar_elemento {lista elemento} {
  foreach item $lista {
    if {$item == $elemento} {
      return $item
    }
  }
  return -1
}

# Buscar un elemento en una lista
elemento_encontrado = [buscar_elemento $frutas fresa]

# Mostrar si el elemento fue encontrado
if {$elemento_encontrado == -1} {
  puts "Fruta no encontrada"
} else {
  puts "Fruta encontrada: $elemento_encontrado"
}
```

Este código muestra cómo utilizar algunas de las características más avanzadas de TCL, incluyendo procedimientos, funciones, clases, diccionarios y búsqueda de elementos en listas.