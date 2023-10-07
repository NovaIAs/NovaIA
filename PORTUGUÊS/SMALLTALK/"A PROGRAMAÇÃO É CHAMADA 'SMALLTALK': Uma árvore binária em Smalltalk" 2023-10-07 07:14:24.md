Claro! Aqui está um código em Smalltalk que cria uma representação visual de uma árvore binária:

```smalltalk
Object subclass: #BinaryTree
    instanceVariableNames: 'value left right'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'BinaryTree'

BinaryTree>>initializeWithValue: aValue
    value := aValue.
    left := nil.
    right := nil.

BinaryTree>>insertValue: aValue
    aValue < value ifTrue: [
        left isNil
            ifTrue: [left := BinaryTree new initializeWithValue: aValue]
            ifFalse: [left insertValue: aValue]
    ] ifFalse: [
        right isNil
            ifTrue: [right := BinaryTree new initializeWithValue: aValue]
            ifFalse: [right insertValue: aValue]
    ].

BinaryTree>>print
    Transcript show: value printString; cr.
    left ifNotNil: [left print].
    right ifNotNil: [right print].
```

Neste código, criamos uma classe chamada `BinaryTree` que representa uma árvore binária. Cada nó da árvore tem um valor, bem como referências para o nó esquerdo e direito.

O método `initializeWithValue:` é usado para inicializar um nó da árvore com um valor específico. O método `insertValue:` é responsável por inserir um novo valor na árvore, mantendo a propriedade de um valor menor à esquerda e um valor maior à direita.

O método `print` é usado para exibir visualmente a árvore binária. Ele imprime o valor do nó atual e, em seguida, chama recursivamente o método `print` nos nós esquerdo e direito, se eles existirem.

Para utilizar essa classe, você pode criar uma instância de `BinaryTree` e inserir valores usando o método `insertValue:`, por exemplo:

```smalltalk
tree := BinaryTree new initializeWithValue: 5.
tree insertValue: 3.
tree insertValue: 7.
tree insertValue: 1.
tree insertValue: 9.
tree print.
```

Isso criará uma árvore binária com os valores 5, 3, 7, 1 e 9, e exibirá a seguinte representação visual:

```
5
3
1
7
9
```

Espero que isso demonstre um exemplo interessante de como usar Smalltalk para criar e manipular uma árvore binária!