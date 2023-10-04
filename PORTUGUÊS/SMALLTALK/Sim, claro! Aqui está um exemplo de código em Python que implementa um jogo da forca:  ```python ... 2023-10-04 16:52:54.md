Claro! Aqui está um exemplo de código em Smalltalk que implementa uma simulação de um zoológico virtual:

```smalltalk
"Definindo as classes"

Object subclass: #Animal
    instanceVariableNames: 'nome especie'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoo'

"Classe Animal"

Animal>>initialize
    super initialize.
    nome := ''.
    especie := ''.

Animal>>setNome: aString
    nome := aString.

Animal>>setEspecie: aString
    especie := aString.

Animal>>nome
    ^ nome.

Animal>>especie
    ^ especie.

"Classe Zoológico"

Object subclass: #Zoologico
    instanceVariableNames: 'animais'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoo'

Zoologico>>initialize
    super initialize.
    animais := OrderedCollection new.

Zoologico>>adicionarAnimal: umAnimal
    animais add: umAnimal.

Zoologico>>listarAnimais
    animais do: [:animal | Transcript show: animal nome; show: ' - '; show: animal especie; cr].

Zoologico>>contarAnimais
    ^ animais size.

"Exemplo de uso"

| zoo |
zoo := Zoologico new.

"Adicionando animais ao zoológico"
zoo adicionarAnimal: (Animal new setNome: 'Leão'; setEspecie: 'Felino').
zoo adicionarAnimal: (Animal new setNome: 'Elefante'; setEspecie: 'Mamífero').
zoo adicionarAnimal: (Animal new setNome: 'Girafa'; setEspecie: 'Herbívoro').

"Listando os animais e contando quantos existem no zoológico"
zoo listarAnimais.
Transcript show: 'Total de animais: ', zoo contarAnimais asString.

Transcript flush.
```

Este código em Smalltalk define duas classes: `Animal` e `Zoologico`. A classe `Animal` possui as variáveis de instância `nome` e `especie`, e métodos para definir e obter o nome e espécie do animal. A classe `Zoologico` possui uma coleção de animais (`animais`) e métodos para adicionar animais à coleção, listar os animais presentes no zoológico e contar quantos animais existem.

No exemplo de uso, criamos um novo zoológico (`zoo`) e adicionamos três animais (leão, elefante e girafa) ao zoológico. Em seguida, listamos os animais presentes no zoológico e exibimos o total de animais.

Espero que este exemplo em Smalltalk tenha sido útil para você!