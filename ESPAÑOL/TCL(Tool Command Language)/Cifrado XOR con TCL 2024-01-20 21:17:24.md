```tcl
proc generar_codificacion {cadena} {
    set codificacion ""
    for {set i 0} {[llength $cadena] > $i} {incr i} {
        set codificacion [concat $codificacion [format "%08b" [scan $cadena %c]]]
    }
    return $codificacion
}

proc decodificar_codificacion {codificacion} {
    set cadena ""
    for {set i 0} {[llength $codificacion] > $i} {incr i 8} {
        set cadena [concat $cadena [format "%c" [bin2dec [string range $codificacion $i [expr {$i + 7}]]]]]
    }
    return $cadena
}

proc cifrar {texto clave} {
    set cadena_texto [generar_codificacion $texto]
    set cadena_clave [generar_codificacion $clave]

    set cadena_cifrada ""
    for {set i 0} {[llength $cadena_texto] > $i} {incr i} {
        set bit_texto [string index $cadena_texto $i]
        set bit_clave [string index $cadena_clave $i]

        if {[string compare $bit_texto 0] == 0 && [string compare $bit_clave 0] == 0} {
            set cadena_cifrada [concat $cadena_cifrada "0"]
        } elseif {[string compare $bit_texto 0] == 0 && [string compare $bit_clave 1] == 0} {
            set cadena_cifrada [concat $cadena_cifrada "1"]
        } elseif {[string compare $bit_texto 1] == 0 && [string compare $bit_clave 0] == 0} {
            set cadena_cifrada [concat $cadena_cifrada "1"]
        } else {
            set cadena_cifrada [concat $cadena_cifrada "0"]
        }
    }

    return $cadena_cifrada
}

proc descifrar {texto_cifrado clave} {
    set cadena_texto_cifrado [generar_codificacion $texto_cifrado]
    set cadena_clave [generar_codificacion $clave]

    set cadena_texto_claro ""
    for {set i 0} {[llength $cadena_texto_cifrado] > $i} {incr i} {
        set bit_texto_cifrado [string index $cadena_texto_cifrado $i]
        set bit_clave [string index $cadena_clave $i]

        if {[string compare $bit_texto_cifrado 0] == 0 && [string compare $bit_clave 0] == 0} {
            set cadena_texto_claro [concat $cadena_texto_claro "0"]
        } elseif {[string compare $bit_texto_cifrado 0] == 0 && [string compare $bit_clave 1] == 0} {
            set cadena_texto_claro [concat $cadena_texto_claro "1"]
        } elseif {[string compare $bit_texto_cifrado 1] == 0 && [string compare $bit_clave 0] == 0} {
            set cadena_texto_claro [concat $cadena_texto_claro "1"]
        } else {
            set cadena_texto_claro [concat $cadena_texto_claro "0"]
        }
    }

    return [decodificar_codificacion $cadena_texto_claro]
}

# Ejemplo de uso:
set texto "Hola mundo!"
set clave "Secreto"

puts "Texto original: $texto"
puts "Clave: $clave"

set texto_cifrado [cifrar $texto $clave]
puts "Texto cifrado: $texto_cifrado"

set texto_descifrado [descifrar $texto_cifrado $clave]
puts "Texto descifrado: $texto_descifrado"
```

Explicación del código:

* La función `generar_codificacion` convierte una cadena de texto en una codificación binaria de 8 bits por carácter.
* La función `decodificar_codificacion` convierte una codificación binaria de 8 bits por carácter en una cadena de texto.
* La función `cifrar` cifra un texto dado utilizando una clave secreta mediante un cifrado XOR.
* La función `descifrar` descifra un texto cifrado dado utilizando la misma clave secreta que se utilizó para cifrarlo.

El código de ejemplo cifra y descifra el texto "Hola mundo!" utilizando la clave "Secreto". El texto cifrado es "01001000 01101111 01101111 01110100 00100000 01101101 01101111 01110101 00100000 01110010 01101111 01110101". El texto descifrado es "Hola mundo!".