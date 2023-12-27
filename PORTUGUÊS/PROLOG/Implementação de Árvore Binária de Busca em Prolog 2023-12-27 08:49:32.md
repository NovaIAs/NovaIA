Claro! Aqui está um código em Prolog que implementa uma árvore binária de busca:

```prolog
% Definindo a estrutura de uma árvore binária
arvore(nil).
arvore(node(Valor, Esquerda, Direita)) :-
    arvore(Esquerda),
    arvore(Direita).

% Inserindo um elemento na árvore binária de busca
inserir(Elemento, nil, node(Elemento, nil, nil)).
inserir(Elemento, node(Valor, Esquerda, Direita), node(Valor, NovaEsquerda, Direita)) :-
    Elemento =< Valor,
    inserir(Elemento, Esquerda, NovaEsquerda).
inserir(Elemento, node(Valor, Esquerda, Direita), node(Valor, Esquerda, NovaDireita)) :-
    Elemento > Valor,
    inserir(Elemento, Direita, NovaDireita).

% Verificando se um elemento está presente na árvore binária de busca
buscar(Elemento, node(Elemento, _, _)).
buscar(Elemento, node(Valor, Esquerda, _)) :-
    Elemento < Valor,
    buscar(Elemento, Esquerda).
buscar(Elemento, node(Valor, _, Direita)) :-
    Elemento > Valor,
    buscar(Elemento, Direita).

% Percorrendo a árvore em ordem
percorrerEmOrdem(nil, []).
percorrerEmOrdem(node(Valor, Esquerda, Direita), Lista) :-
    percorrerEmOrdem(Esquerda, ListaEsquerda),
    percorrerEmOrdem(Direita, ListaDireita),
    append(ListaEsquerda, [Valor|ListaDireita], Lista).

% Exemplo de uso
:- initialization(main).
main :-
    arvore(Arvore),
    inserir(5, Arvore, Arvore1),
    inserir(3, Arvore1, Arvore2),
    inserir(8, Arvore2, Arvore3),
    inserir(1, Arvore3, Arvore4),
    inserir(4, Arvore4, Arvore5),
    inserir(7, Arvore5, Arvore6),
    inserir(9, Arvore6, Arvore7),
    inserir(2, Arvore7, ArvoreFinal),
    write('Árvore resultante: '),
    write(ArvoreFinal),
    nl,
    write('Elementos em ordem: '),
    percorrerEmOrdem(ArvoreFinal, Lista),
    write(Lista),
    nl,
    halt.
```

Neste código, definimos a estrutura de uma árvore binária em Prolog com dois tipos de nós: um nó vazio (`nil`) e um nó com um valor e subárvores esquerda e direita (`node(Valor, Esquerda, Direita)`).

Em seguida, temos o predicado `inserir/3`, que permite a inserção de elementos na árvore binária de busca. Ele verifica se o elemento a ser inserido é menor ou igual ao valor do nó atual e, nesse caso, faz a inserção na subárvore esquerda. Caso contrário, faz a inserção na subárvore direita. Esse processo ocorre recursivamente até encontrar um nó vazio, onde o novo elemento será inserido.

O predicado `buscar/2` é responsável por verificar se um elemento está presente na árvore binária de busca. Ele verifica se o elemento buscado é igual ao valor do nó atual e, caso contrário, faz a busca recursivamente nas subárvores esquerda ou direita, dependendo do valor do elemento.

Por fim, o predicado `percorrerEmOrdem/2` percorre a árvore binária em ordem e retorna uma lista com os elementos em ordem crescente. Ele faz a chamada recursiva passando primeiro pela subárvore esquerda, em seguida adiciona o valor do nó atual à lista e, por último, faz a chamada recursiva passando pela subárvore direita.

No exemplo de uso, criamos uma árvore vazia (`arvore(Arvore)`), inserimos alguns elementos (5, 3, 8, 1, 4, 7, 9, 2) na árvore e exibimos a árvore resultante e os elementos em ordem.