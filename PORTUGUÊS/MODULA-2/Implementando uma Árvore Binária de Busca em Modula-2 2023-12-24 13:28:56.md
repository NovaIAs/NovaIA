Claro! Aqui está um código em Modula-2 que implementa uma árvore binária de busca. Essa estrutura de dados é usada para armazenar elementos de forma organizada, permitindo uma busca eficiente. O código é escrito em português e está bem detalhado para facilitar a compreensão:

```
MODULE ArvoreBinariaDeBusca;

  (* Definindo o tipo de dado armazenado na árvore *)
  TYPE
    Elemento = INTEGER;

  (* Definindo o tipo de nó da árvore *)
  TYPE
    No = POINTER TO Nodo;
    Nodo = RECORD
      Valor: Elemento;
      Esquerda, Direita: No;
    END;

  (* Função para criar um novo nó *)
  PROCEDURE NovoNo(valor: Elemento): No;
    VAR
      novoNo: No;
    BEGIN
      NEW(novoNo);
      novoNo.Valor := valor;
      novoNo.Esquerda := NIL;
      novoNo.Direita := NIL;
      RETURN novoNo;
    END NovoNo;

  (* Função para inserir um elemento na árvore *)
  PROCEDURE Inserir(VAR raiz: No; valor: Elemento);
    BEGIN
      IF raiz = NIL THEN
        raiz := NovoNo(valor);
      ELSIF valor < raiz.Valor THEN
        Inserir(raiz.Esquerda, valor);
      ELSE
        Inserir(raiz.Direita, valor);
      END;
    END Inserir;

  (* Função para buscar um elemento na árvore *)
  FUNCTION Buscar(raiz: No; valor: Elemento): BOOLEAN;
    BEGIN
      IF raiz = NIL THEN
        RETURN FALSE;
      ELSIF valor = raiz.Valor THEN
        RETURN TRUE;
      ELSIF valor < raiz.Valor THEN
        RETURN Buscar(raiz.Esquerda, valor);
      ELSE
        RETURN Buscar(raiz.Direita, valor);
      END;
    END Buscar;

  (* Função para imprimir a árvore em ordem *)
  PROCEDURE ImprimirEmOrdem(raiz: No);
    BEGIN
      IF raiz <> NIL THEN
        ImprimirEmOrdem(raiz.Esquerda);
        Write(raiz.Valor, ' ');
        ImprimirEmOrdem(raiz.Direita);
      END;
    END ImprimirEmOrdem;

  (* Função principal *)
  PROCEDURE Principal;
    VAR
      arvore: No;
    BEGIN
      arvore := NIL;

      (* Inserindo elementos na árvore *)
      Inserir(arvore, 8);
      Inserir(arvore, 3);
      Inserir(arvore, 10);
      Inserir(arvore, 1);
      Inserir(arvore, 6);
      Inserir(arvore, 14);
      Inserir(arvore, 4);
      Inserir(arvore, 7);
      Inserir(arvore, 13);

      (* Imprimindo a árvore em ordem *)
      ImprimirEmOrdem(arvore);
    END Principal;

BEGIN
  Principal;
END ArvoreBinariaDeBusca.
```

Neste código, criamos um módulo chamado "ArvoreBinariaDeBusca" que implementa uma árvore binária de busca em Modula-2. A árvore é composta por nós, onde cada nó possui um valor, um ponteiro para o nó esquerdo e um ponteiro para o nó direito. A árvore começa com um nó raiz, que é o primeiro elemento inserido. 

A função "NovoNo" é responsável por criar um novo nó e atribuir o valor passado como parâmetro. Os ponteiros para os nós esquerdo e direito são inicializados como NIL, indicando que não há nós conectados a eles. 

A função "Inserir" é usada para inserir um elemento na árvore. Se a árvore estiver vazia (raiz = NIL), um novo nó será criado com o valor passado. Caso contrário, o valor será comparado com o valor do nó atual. Se for menor, a função será chamada recursivamente para inserir o valor na subárvore esquerda. Caso contrário, a função será chamada recursivamente para inserir o valor na subárvore direita.

A função "Buscar" é usada para buscar um elemento na árvore. A busca é feita de forma recursiva, comparando o valor com o valor do nó atual. Se o valor for igual ao valor do nó atual, a função retorna TRUE. Se o valor for menor, a função é chamada recursivamente para buscar na subárvore esquerda. Caso contrário, a função é chamada recursivamente para buscar na subárvore direita. Se o nó atual for NIL, ou seja, a busca chegou a uma folha da árvore sem encontrar o valor, a função retorna FALSE.

A função "ImprimirEmOrdem" é usada para imprimir os elementos da árvore em ordem. A impressão é feita de forma recursiva, primeiro imprimindo a subárvore esquerda, seguida pelo valor do nó atual e, por fim, a subárvore direita.

A função "Principal" é a função principal do módulo. Nela, criamos uma árvore vazia e inserimos alguns elementos. Em seguida, chamamos a função "ImprimirEmOrdem" para imprimir a árvore em ordem.

Espero que esse código atenda às suas expectativas!