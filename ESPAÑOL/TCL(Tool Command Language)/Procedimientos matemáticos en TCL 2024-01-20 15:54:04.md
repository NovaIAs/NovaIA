```tcl
# Definición de procedimientos

proc saludo {nombre} {
    puts "Hola, $nombre!"
}

proc suma {a b} {
    expr $a + $b
}

proc resta {a b} {
    expr $a - $b
}

proc multiplicacion {a b} {
    expr $a * $b
}

proc division {a b} {
    expr $a / $b
}

# Definición de variables

set a 10
set b 5

# Uso de los procedimientos

saludo "Juan"
puts [suma $a $b]
puts [resta $a $b]
puts [multiplicacion $a $b]
puts [division $a $b]
```

Este código define cinco procedimientos: `saludo`, `suma`, `resta`, `multiplicacion` y `division`. Luego, define dos variables, `a` y `b`, y las usa para llamar a los procedimientos y mostrar los resultados en la consola.

Una explicación detallada del código:

* El primer procedimiento, `saludo`, toma un argumento, `nombre`, y muestra un mensaje de saludo.
* El segundo procedimiento, `suma`, toma dos argumentos, `a` y `b`, y devuelve su suma.
* El tercer procedimiento, `resta`, toma dos argumentos, `a` y `b`, y devuelve su diferencia.
* El cuarto procedimiento, `multiplicacion`, toma dos argumentos, `a` y `b`, y devuelve su producto.
* El quinto procedimiento, `division`, toma dos argumentos, `a` y `b`, y devuelve su cociente.

Las variables `a` y `b` se definen con los valores 10 y 5, respectivamente.

Luego, el código llama a los procedimientos `saludo`, `suma`, `resta`, `multiplicacion` y `division`, y muestra los resultados en la consola.

El resultado del código será:

```
Hola, Juan!
15
5
50
2
```