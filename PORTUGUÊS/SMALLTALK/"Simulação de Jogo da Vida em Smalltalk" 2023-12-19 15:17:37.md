Claro! Aqui está um exemplo de código em Smalltalk que cria uma simulação de um jogo de tabuleiro chamado "Jogo da Vida":

```
"Definindo a classe do jogo"
Object subclass: #JogoDaVida
    instanceVariableNames: 'tabuleiro'
    classVariableNames: ''
    poolDictionaries: ''

"Definindo os métodos da classe"
JogoDaVida>>initialize
    "Inicializa o jogo com um tabuleiro vazio"
    super initialize.
    tabuleiro := Array new: 10.
    1 to: 10 do: [ :linha |
        tabuleiro at: linha put: (Array new: 10).
        1 to: 10 do: [ :coluna |
            tabuleiro at: linha at: coluna put: false
        ]
    ].

JogoDaVida>>exibir
    "Exibe o estado atual do tabuleiro"
    tabuleiro do: [ :linha |
        linha do: [ :coluna |
            coluna ifTrue: [ Transcript show: ' X' ]
                    ifFalse: [ Transcript show: ' O' ]
        ].
        Transcript cr
    ].

JogoDaVida>>evoluir
    "Aplica as regras do jogo para a próxima geração do tabuleiro"
    | novoTabuleiro |
    novoTabuleiro := tabuleiro copy.
    1 to: 10 do: [ :linha |
        1 to: 10 do: [ :coluna |
            | vizinhos |
            vizinhos := self contarVizinhosVivosParaLinha: linha coluna: coluna.
            ((tabuleiro at: linha at: coluna) and: [ vizinhos ~= 2 and: [ vizinhos ~= 3 ] ])
                ifTrue: [ novoTabuleiro at: linha at: coluna put: false ]
                ifFalse: [ (tabuleiro at: linha at: coluna) not
                            ifTrue: [ novoTabuleiro at: linha at: coluna put: vizinhos = 3 ]
                            ifFalse: [ novoTabuleiro at: linha at: coluna put: true ]
                          ]
        ]
    ].
    tabuleiro := novoTabuleiro.

JogoDaVida>>contarVizinhosVivosParaLinha: linha coluna: coluna
    "Conta quantos vizinhos vivos uma célula tem"
    | vizinhos |
    vizinhos := 0.
    (linha > 1) ifTrue: [
        coluna > 1 ifTrue: [ tabuleiro at: linha - 1 at: coluna - 1 ifTrue: [ vizinhos := vizinhos + 1 ] ].
        tabuleiro at: linha - 1 at: coluna ifTrue: [ vizinhos := vizinhos + 1 ].
        coluna < 10 ifTrue: [ tabuleiro at: linha - 1 at: coluna + 1 ifTrue: [ vizinhos := vizinhos + 1 ] ]
    ].
    coluna > 1 ifTrue: [ tabuleiro at: linha at: coluna - 1 ifTrue: [ vizinhos := vizinhos + 1 ] ].
    coluna < 10 ifTrue: [ tabuleiro at: linha at: coluna + 1 ifTrue: [ vizinhos := vizinhos + 1 ] ].
    (linha < 10) ifTrue: [
        coluna > 1 ifTrue: [ tabuleiro at: linha + 1 at: coluna - 1 ifTrue: [ vizinhos := vizinhos + 1 ] ].
        tabuleiro at: linha + 1 at: coluna ifTrue: [ vizinhos := vizinhos + 1 ].
        coluna < 10 ifTrue: [ tabuleiro at: linha + 1 at: coluna + 1 ifTrue: [ vizinhos := vizinhos + 1 ] ]
    ].
    ^ vizinhos

"Utilizando o jogo"
jogo := JogoDaVida new.
jogo exibir.

"Definindo células vivas no tabuleiro"
jogo tabuleiro at: 3 at: 4 put: true.
jogo tabuleiro at: 3 at: 5 put: true.
jogo tabuleiro at: 3 at: 6 put: true.
jogo tabuleiro at: 4 at: 5 put: true.
jogo tabuleiro at: 5 at: 4 put: true.
jogo tabuleiro at: 5 at: 5 put: true.
jogo tabuleiro at: 5 at: 6 put: true.

jogo exibir.

"Simulando várias gerações do jogo"
10 timesRepeat: [
    jogo evoluir.
    jogo exibir.
    Delay waitForSeconds: 1
].
```

Neste código, criei uma classe chamada `JogoDaVida` que representa o jogo. O jogo consiste em um tabuleiro de 10x10 células, onde cada célula pode estar viva ou morta. O método `initialize` inicializa o tabuleiro com todas as células mortas. O método `exibir` exibe no console o estado atual do tabuleiro, usando "X" para células vivas e "O" para células mortas. O método `evoluir` aplica as regras do jogo para a próxima geração do tabuleiro, considerando os vizinhos de cada célula. O método `contarVizinhosVivosParaLinha:coluna:` conta quantos vizinhos vivos uma célula tem. Por fim, o código cria uma instância do jogo, define algumas células vivas e simula 10 gerações do jogo, exibindo o estado do tabuleiro a cada geração.