```smalltalk
;; Este código crea una clase llamada "Persona" con atributos de nombre, edad y género.

clase Persona
    nombre: 'Juan'
    edad: 25
    género: 'Masculino'
    inic
        super inic
    ;; Define un método llamado "saludar" que imprime un saludo con el nombre de la persona.
    método saludar
        | saludo |
        saludo := 'Hola, mi nombre es ' + nombre.
        print(saludo)
    ;; Define un método llamado "cambiarNombre" que cambia el nombre de la persona.
    método cambiarNombre: nuevoNombre
        nombre := nuevoNombre
    ;; Define un método llamado "aumentarEdad" que aumenta la edad de la persona en un año.
    método aumentarEdad
        edad := edad + 1
end

;; Crea una instancia de la clase Persona llamada "juan" con el nombre "Juan", edad 25 y género "Masculino".
juan := Persona nuevo.

;; Imprime un saludo usando el método "saludar".
juan saludar.
;; Cambia el nombre de "Juan" a "María" usando el método "cambiarNombre".
juan cambiarNombre: 'María'.
;; Aumenta la edad de "María" en un año usando el método "aumentarEdad".
juan aumentarEdad.

;; Imprime un saludo usando el método "saludar".
juan saludar.
```

Explicación del código:

1. `clase Persona`: Esta línea define una nueva clase llamada "Persona".

2. `nombre: 'Juan'`: Esta línea establece el valor inicial del atributo `nombre` de la clase `Persona` a "Juan".

3. `edad: 25`: Esta línea establece el valor inicial del atributo `edad` de la clase `Persona` a 25.

4. `género: 'Masculino'`: Esta línea establece el valor inicial del atributo `género` de la clase `Persona` a "Masculino".

5. `inic`: Este método es el inicializador de la clase `Persona`. Se ejecuta cuando se crea una nueva instancia de la clase. En este caso, llama al inicializador de la clase padre (`super inic`) y no hace nada más.

6. `método saludar`: Esta línea define un nuevo método llamado "saludar" en la clase `Persona`.

7. `| saludo |`: Esta línea define una variable local llamada `saludo`.

8. `saludo := 'Hola, mi nombre es ' + nombre`: Esta línea construye una cadena de texto que contiene el saludo "Hola, mi nombre es" seguido del valor del atributo `nombre` de la instancia actual de la clase `Persona`.

9. `print(saludo)`: Esta línea imprime el valor de la variable `saludo` en la consola.

10. `método cambiarNombre: nuevoNombre`: Esta línea define un nuevo método llamado "cambiarNombre" en la clase `Persona`. Este método toma un argumento llamado `nuevoNombre`.

11. `nombre := nuevoNombre`: Esta línea cambia el valor del atributo `nombre` de la instancia actual de la clase `Persona` al valor de `nuevoNombre`.

12. `método aumentarEdad`: Esta línea define un nuevo método llamado "aumentarEdad" en la clase `Persona`.

13. `edad := edad + 1`: Esta línea aumenta el valor del atributo `edad` de la instancia actual de la clase `Persona` en uno.

14. `juan := Persona nuevo`: Esta línea crea una nueva instancia de la clase `Persona` llamada "juan".

15. `juan saludar`: Esta línea llama al método "saludar" de la instancia "juan" de la clase `Persona`.

16. `juan cambiarNombre: 'María'`: Esta línea llama al método "cambiarNombre" de la instancia "juan" de la clase `Persona` y le pasa el nuevo nombre "María".

17. `juan aumentarEdad`: Esta línea llama al método "aumentarEdad" de la instancia "juan" de la clase `Persona`.

18. `juan saludar`: Esta línea llama al método "saludar" de la instancia "juan" de la clase `Persona`.