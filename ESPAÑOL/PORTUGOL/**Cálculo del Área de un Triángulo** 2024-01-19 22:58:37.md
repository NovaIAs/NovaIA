```portuguol
programa CalculaAreaTriangulo {

    funcao areaTriangulo(base: real, altura: real): real {
        retorna (base * altura) / 2
    }

    funcao main() {
        real base, altura, area
        escreva("\nInforme a base do triângulo: ")
        leia(base)
        escreva("\nInforme a altura do triângulo: ")
        leia(altura)

        area = areaTriangulo(base, altura)

        escreva("\nÁrea do triângulo: ", area)

        se (area > 100) {
            escreva("\nTriângulo grande")
        }
        senao {
            escreva("\nTriângulo pequeno")
        }

        escreva("\n\n")
    }

}
```

Explicación:
- programa CalculaAreaTriangulo: Este es el nombre del programa en PORTUGOL.
- funcao areaTriangulo(base: real, altura: real): real: Esta es una función que calcula el área de un triángulo dada su base y altura. La función devuelve el área del triángulo.
- funcao main(): Esta es la función principal del programa, donde se ejecuta el código principal.
- real base, altura, area: Estas son las variables reales utilizadas para almacenar la base, la altura y el área del triángulo.
- escreva("\nInforme a base do triângulo: "): Esta línea muestra un mensaje en la consola solicitando al usuario que ingrese la base del triángulo.
- leia(base): Esta línea lee la entrada del usuario y la almacena en la variable base.
- escreva("\nInforme a altura do triângulo: "): Esta línea muestra un mensaje en la consola solicitando al usuario que ingrese la altura del triángulo.
- leia(altura): Esta línea lee la entrada del usuario y la almacena en la variable altura.
- area = areaTriangulo(base, altura): Esta línea calcula el área del triángulo utilizando la función areaTriangulo() y almacena el resultado en la variable area.
- escreva("\nÁrea do triângulo: ", area): Esta línea muestra un mensaje en la consola con el área del triángulo.
- se (area > 100) { ... }: Esta es una instrucción condicional que verifica si el área del triángulo es mayor que 100. Si es así, se ejecuta el bloque de código dentro del cuerpo del if.
- escreva("\nTriângulo grande"): Esta línea muestra un mensaje en la consola indicando que el triángulo es grande.
- }senao { ... }: Esta es una instrucción else que se ejecuta si el área del triángulo no es mayor que 100.
- escreva("\nTriângulo pequeno"): Esta línea muestra un mensaje en la consola indicando que el triángulo es pequeño.
- }
- escreva("\n\n"): Esta línea muestra 2 líneas en blanco en la consola.