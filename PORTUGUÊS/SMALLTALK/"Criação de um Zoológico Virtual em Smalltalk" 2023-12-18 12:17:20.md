Claro! Aqui está um exemplo de código em Smalltalk que cria uma simulação de um zoológico virtual:

```smalltalk
"Definindo a classe Animal"
Object subclass: #Animal
    instanceVariableNames: 'nome especie'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoo'

"Implementando o método de inicialização da classe Animal"
Animal>>initialize
    super initialize.
    nome := ''.
    especie := ''.

"Implementando os métodos de acesso para nome e espécie"
Animal>>nome: aString
    nome := aString.

Animal>>nome
    ^ nome.

Animal>>especie: aString
    especie := aString.

Animal>>especie
    ^ especie.

"Definindo a classe Zoo"
Object subclass: #Zoo
    instanceVariableNames: 'animais'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoo'

"Implementando o método de inicialização da classe Zoo"
Zoo>>initialize
    super initialize.
    animais := OrderedCollection new.

"Implementando o método para adicionar um animal ao zoológico"
Zoo>>adicionarAnimal: umAnimal
    animais add: umAnimal.

"Implementando o método para remover um animal do zoológico"
Zoo>>removerAnimal: umAnimal
    animais remove: umAnimal.

"Implementando o método para listar todos os animais do zoológico"
Zoo>>listarAnimais
    animais do: [ :animal |
        Transcript show: 'Nome: ', animal nome, '   Espécie: ', animal especie; cr ].

"Programa principal"
zoo := Zoo new.

leao := Animal new.
leao nome: 'Leão'.
leao especie: 'Felino'.
zoo adicionarAnimal: leao.

girafa := Animal new.
girafa nome: 'Girafa'.
girafa especie: 'Herbívoro'.
zoo adicionarAnimal: girafa.

elefante := Animal new.
elefante nome: 'Elefante'.
elefante especie: 'Herbívoro'.
zoo adicionarAnimal: elefante.

zoo listarAnimais.
```

Neste código, criamos duas classes: `Animal` e `Zoo`. A classe `Animal` possui os atributos `nome` e `especie`, bem como métodos de acesso para esses atributos. A classe `Zoo` possui uma coleção de animais e métodos para adicionar, remover e listar os animais do zoológico.

No programa principal, criamos um novo zoológico (`zoo`) e alguns animais (`leao`, `girafa` e `elefante`). Em seguida, adicionamos esses animais ao zoológico e chamamos o método `listarAnimais` para exibir os detalhes de cada animal no console.

Espero que este exemplo tenha sido útil e atenda às suas expectativas!