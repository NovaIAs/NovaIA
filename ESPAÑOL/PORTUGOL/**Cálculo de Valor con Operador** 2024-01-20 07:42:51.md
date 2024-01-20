```PORTUGOL

Função CalcularValor (Valor: Real, Operador: Caractere): Real
Real: Retorno
    Se Operador = "+"
        Retorno = Valor + 10
    Senão se Operador = "-"
        Retorno = Valor - 10
    Senão
        Retorno = Valor
    FimSe
FimFunção


Início
    ValorReal: Real
    Operador: Caractere

    Escreva ("Digite um valor real: ")
    Leia (ValorReal)

    Escreva ("Digite um operador (+ ou -): ")
    Leia (Operador)

    ValorCalculado = CalcularValor (ValorReal, Operador)

    Escreva ("O valor calculado é: ", ValorCalculado)
Fim

```

Explicación:

Este código en PORTUGOL realiza la siguiente tarea:
1. Solicita al usuario que ingrese un valor real (un número con decimales) y un operador (suma (+) o resta (-)).
2. Llama a la función "CalcularValor" con el valor real y el operador ingresados por el usuario.
3. La función "CalcularValor" realiza la operación indicada por el operador sobre el valor real.
4. El resultado de la operación se almacena en la variable "ValorCalculado".
5. Finalmente, se muestra en pantalla el valor calculado.

Este código demuestra el uso de funciones en PORTUGOL, que permiten dividir el programa en módulos más pequeños y reutilizables. La función "CalcularValor" es un ejemplo de una función que realiza una tarea específica y puede ser llamada desde diferentes partes del programa.