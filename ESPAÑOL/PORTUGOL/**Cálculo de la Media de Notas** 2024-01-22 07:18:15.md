```portuguol
programa calcula_media_notas

definir notas como inteiro
definir media_notas como numérico
definir num_notas como inteiro
definir soma_notas como numérico
definir i como inteiro

escreva("Quantas notas deseja inserir?")
leia(num_notas)

para i de 1 até num_notas faca
    escreval("Nota ", i, ":")
    leia(notas)
    soma_notas <- soma_notas + notas
fim_para

media_notas <- soma_notas / num_notas

se media_notas >= 60 então
    escreval("Aluno aprovado!")
senão
    escreval("Aluno reprovado!")
fim_se

fim_programa
```

**Explicación del código:**

* El programa comienza por definir las variables que se van a utilizar.

* **notas:** Esta variable se utiliza para almacenar cada una de las notas del alumno.

* **media_notas:** Esta variable se utiliza para almacenar la media de las notas del alumno.

* **num_notas:** Esta variable se utiliza para almacenar el número de notas que se van a introducir.

* **soma_notas:** Esta variable se utiliza para almacenar la suma de las notas del alumno.

* **i:** Esta variable se utiliza para recorrer el bucle que se utiliza para introducir las notas del alumno.

* El programa entonces utiliza un bucle `para` para pedir al usuario que introduzca las notas del alumno.

* **escreval:** Esta función se utiliza para mostrar un mensaje al usuario.

* **leia:** Esta función se utiliza para leer un valor del usuario.

* **soma_notas <- soma_notas + notas:** Esta línea de código se utiliza para ir sumando las notas del alumno.

* El programa entonces calcula la media de las notas del alumno dividiendo la suma de las notas por el número de notas.

* **media_notas <- soma_notas / num_notas:** Esta línea de código se utiliza para calcular la media de las notas del alumno.

* El programa entonces utiliza un `se` para comprobar si la media de las notas del alumno es mayor o igual a 60. Si es así, el programa muestra un mensaje diciendo que el alumno está aprobado. Si no, el programa muestra un mensaje diciendo que el alumno está reprobado.

* **se media_notas >= 60 então:** Esta línea de código comprueba si la media de las notas del alumno es mayor o igual a 60.

* **escreval:** Esta función se utiliza para mostrar un mensaje al usuario.

* **senão:** Esta palabra clave se utiliza para especificar el código que se debe ejecutar si la condición del `se` es falsa.

* **escreval:** Esta función se utiliza para mostrar un mensaje al usuario.

* El programa entonces termina.