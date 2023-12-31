Claro! Aqui está um código complexo em Visual Basic que atende aos seus requisitos:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.Threading.Tasks

Module Program
    Sub Main(args As String())
        Console.WriteLine("Bem-vindo ao programa complexo em Visual Basic!")

        ' Declaração de variáveis
        Dim numero1, numero2, resultado As Double
        Dim operacao As String

        ' Loop principal do programa
        While True
            Console.WriteLine("Digite o primeiro número: ")
            numero1 = Double.Parse(Console.ReadLine())

            Console.WriteLine("Digite o segundo número: ")
            numero2 = Double.Parse(Console.ReadLine())

            Console.WriteLine("Escolha uma operação:")
            Console.WriteLine("1 - Soma")
            Console.WriteLine("2 - Subtração")
            Console.WriteLine("3 - Multiplicação")
            Console.WriteLine("4 - Divisão")
            Console.WriteLine("5 - Sair")

            operacao = Console.ReadLine()

            Select Case operacao
                Case "1"
                    resultado = numero1 + numero2
                    Console.WriteLine("O resultado da soma é: " & resultado)
                Case "2"
                    resultado = numero1 - numero2
                    Console.WriteLine("O resultado da subtração é: " & resultado)
                Case "3"
                    resultado = numero1 * numero2
                    Console.WriteLine("O resultado da multiplicação é: " & resultado)
                Case "4"
                    If numero2 <> 0 Then
                        resultado = numero1 / numero2
                        Console.WriteLine("O resultado da divisão é: " & resultado)
                    Else
                        Console.WriteLine("Erro: Não é possível dividir por zero.")
                    End If
                Case "5"
                    Console.WriteLine("Encerrando o programa...")
                    Exit While
                Case Else
                    Console.WriteLine("Opção inválida! Tente novamente.")
            End Select

            Console.WriteLine() ' Linha em branco para melhorar a legibilidade

        End While
    End Sub
End Module
```

Explicação do código:

1. Começamos importando as bibliotecas necessárias para o programa.
2. Em seguida, definimos um módulo chamado "Program".
3. Dentro do módulo, temos o método principal "Main" que será executado quando o programa for iniciado.
4. Imprimimos uma mensagem de boas-vindas ao usuário.
5. Em seguida, declaramos as variáveis `numero1`, `numero2` e `resultado` como tipo `Double`, para armazenar os números e o resultado das operações.
6. Também declaramos a variável `operacao` como tipo `String`, para armazenar a escolha da operação pelo usuário.
7. Iniciamos um loop infinito usando o comando `While True`.
8. Dentro do loop, solicitamos ao usuário que digite o primeiro e o segundo número.
9. Em seguida, exibimos um menu com as opções de operação.
10. O usuário escolhe uma opção digitando o número correspondente, e usamos a instrução `Select Case` para executar o código correspondente a essa opção.
11. No caso de uma operação válida, realizamos a operação matemática apropriada e exibimos o resultado na tela.
12. Caso o usuário escolha a opção "5", exibimos uma mensagem de encerramento e saímos do loop.
13. Caso o usuário digite uma opção inválida, exibimos uma mensagem de erro.
14. Por fim, adicionamos uma linha em branco para melhorar a legibilidade e o loop continua até que o usuário escolha a opção "5".

Espero que isso atenda às suas expectativas!