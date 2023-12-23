Aqui está um exemplo de código em Smalltalk que implementa um simulador de vida marinha. Neste simulador, cada organismo é representado por uma classe, com comportamentos e características específicas.

```smalltalk
"Definindo a classe Organismo, que representa um organismo vivo"
Object subclass: #Organismo
    instanceVariableNames: 'posicao energia'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'SimuladorVidaMarinha'

"Definindo os métodos para a classe Organismo"
Organismo>>initialize
    "Inicializa a posição e a energia do organismo"
    posicao := 0.
    energia := 100.

Organismo>>mover: distancia
    "Move o organismo para a posição especificada"
    posicao := posicao + distancia.

Organismo>>comer: quantidade
    "Aumenta a energia do organismo após se alimentar"
    energia := energia + quantidade.

Organismo>>reproduzirCom: organismo
    "Cria um novo organismo com base nas características do organismo atual e do parceiro"
    | filho |
    filho := Organismo new.
    filho posicao: posicao.
    filho energia: (energia + organismo energia) / 2.
    ^ filho.

"Definindo a classe Peixe, que representa um peixe no simulador"
Organismo subclass: #Peixe
    instanceVariableNames: 'cor'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'SimuladorVidaMarinha'

"Definindo os métodos para a classe Peixe"
Peixe>>initialize
    "Inicializa a cor do peixe"
    cor := 'azul'.

Peixe>>nadar: distancia
    "Move o peixe para a posição especificada"
    self mover: distancia.

Peixe>>mudarCor: novaCor
    "Altera a cor do peixe"
    cor := novaCor.

"Definindo a classe Alga, que representa uma alga no simulador"
Organismo subclass: #Alga
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'SimuladorVidaMarinha'

"Definindo os métodos para a classe Alga"
Alga>>fotossintese
    "Realiza a fotossíntese para produzir energia"
    self comer: 10.

"Exemplo de uso do simulador de vida marinha"
| peixe1 peixe2 alga1 alga2 filho |
peixe1 := Peixe new.
peixe1 nadar: 10.
peixe1 mudarCor: 'vermelho'.

peixe2 := Peixe new.
peixe2 nadar: 20.
peixe2 mudarCor: 'amarelo'.

alga1 := Alga new.
alga1 fotossintese.

alga2 := Alga new.
alga2 fotossintese.

filho := peixe1 reproduzirCom: peixe2.

Transcript show: 'Peixe 1 está na posição: ', peixe1 posicao asString; cr.
Transcript show: 'Peixe 2 está na posição: ', peixe2 posicao asString; cr.
Transcript show: 'Alga 1 tem energia: ', alga1 energia asString; cr.
Transcript show: 'Alga 2 tem energia: ', alga2 energia asString; cr.
Transcript show: 'Filho do peixe 1 e peixe 2 tem energia: ', filho energia asString; cr.
```

Neste código, temos a definição das classes `Organismo`, `Peixe` e `Alga`. A classe `Organismo` serve como uma classe base, enquanto as classes `Peixe` e `Alga` herdam suas características e comportamentos.

Cada organismo tem uma posição e uma energia, que são inicializadas no método `initialize`. O método `mover: distancia` permite que o organismo se mova para uma posição específica, aumentando a sua posição na distância fornecida. O método `comer: quantidade` aumenta a energia do organismo após se alimentar.

A classe `Peixe` adiciona características específicas para representar um peixe. O método `nadar: distancia` permite que o peixe se mova na água, utilizando o método `mover` herdado da classe `Organismo`. O método `mudarCor: novaCor` permite alterar a cor do peixe.

A classe `Alga` representa uma alga e possui o método `fotossintese` que simula a produção de energia por meio da fotossíntese.

No exemplo de uso do simulador, são criados dois peixes, duas algas e um filho do peixe 1 e peixe 2. Os métodos `nad