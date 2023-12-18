Claro! Aqui está um exemplo de código em Smalltalk que cria uma calculadora básica com uma interface gráfica:

```smalltalk
"Primeiro, vamos criar a janela principal da calculadora"
MainWindow := SystemWindow new.
MainWindow label: 'Calculadora'.
MainWindow extent: 300 @ 400.

"Em seguida, criamos os botões numéricos e de operações"
numeros := #(1 2 3 4 5 6 7 8 9 0).
operacoes := #(+ - * /).

numeros do: [:numero |
    button := ButtonMorph new.
    button label: numero asString.
    button action: [Transcript show: numero asString].
    button position: (120@(260 - (numeros indexOf: numero // 3) * 40)).
    MainWindow addMorph: button].

operacoes do: [:operacao |
    button := ButtonMorph new.
    button label: operacao.
    button action: [Transcript show: operacao].
    button position: (220@(260 - (operacoes indexOf: operacao) * 40)).
    MainWindow addMorph: button].

"Por último, adicionamos a janela principal à tela"
MainWindow open.
```

Explicação do código:
- Primeiro, criamos a janela principal da calculadora usando a classe SystemWindow e definimos seu rótulo e tamanho.
- Em seguida, criamos os botões numéricos e de operações usando a classe ButtonMorph. Iteramos sobre a coleção `numeros` para criar os botões numéricos e definimos a ação de cada botão para imprimir o número correspondente no Transcript. Da mesma forma, iteramos sobre a coleção `operacoes` para criar os botões de operações.
- Por fim, adicionamos os botões à janela principal usando o método `addMorph:` e abrimos a janela com o método `open`.

Esse código cria uma calculadora simples com botões numéricos de 0 a 9 e botões de operações básicas (+, -, *, /). Quando você clicar em um botão, o número ou operação correspondente será exibido no Transcript. Vale ressaltar que o código pode ser personalizado e expandido de acordo com suas necessidades.