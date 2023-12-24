Com certeza! Aqui está um código complexo em Visual Basic que atende aos seus requisitos:

```vb
Imports System

Module Program
    Sub Main()
        Console.WriteLine("Bem-vindo ao programa complexo em Visual Basic!")

        ' Declaração de variáveis
        Dim numero As Integer = 0
        Dim soma As Integer = 0
        Dim media As Double = 0.0
        Dim contador As Integer = 0

        ' Laço de repetição
        For i As Integer = 1 To 10
            Console.Write("Digite o {0}º número: ", i)
            numero = Convert.ToInt32(Console.ReadLine())

            ' Verifica se o número é par
            If numero Mod 2 = 0 Then
                soma += numero
                contador += 1
            End If
        Next

        ' Cálculo da média
        If contador > 0 Then
            media = soma / contador
        End If

        ' Exibição dos resultados
        Console.WriteLine("A soma dos números pares é: " & soma)
        Console.WriteLine("A média dos números pares é: " & media)

        Console.ReadLine()
    End Sub
End Module
```

Neste código, você encontrará um programa que solicita ao usuário a entrada de 10 números inteiros. Em seguida, ele verifica se cada número é par e, caso seja, realiza a soma desses números. Além disso, o código também calcula a média dos números pares.

Aqui estão os principais elementos utilizados no código:

- `Imports System`: Importa o namespace `System`, permitindo o uso das classes definidas nele, como `Console` e `Convert`.
- `Module Program`: Define um módulo chamado "Program", que é o ponto de entrada do programa.
- `Sub Main()`: Define o método principal do programa.
- `Console.WriteLine()`: Exibe uma mensagem na saída padrão.
- `Dim`: Declaração de variáveis.
- `For`: Laço de repetição.
- `Convert.ToInt32()`: Converte uma string em um número inteiro.
- `Mod`: Operador que retorna o resto da divisão entre dois números.
- `If`: Estrutura condicional para verificar se um número é par.
- `&`: Operador de concatenação de strings.
- `Console.ReadLine()`: Lê uma linha de texto da entrada padrão.

Espero que este código atenda às suas expectativas!