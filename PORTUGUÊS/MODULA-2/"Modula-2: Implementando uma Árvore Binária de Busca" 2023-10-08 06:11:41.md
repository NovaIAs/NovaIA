Claro! Aqui está um exemplo de um código em Modula-2 que implementa uma árvore binária de busca:

```
MODULE ArvoreBinariaDeBusca;

  TYPE
    No = POINTER TO Nodo;
    Nodo = RECORD
      valor: INTEGER;
      esquerda, direita: No;
    END;

  VAR
    raiz: No;

  PROCEDURE Inserir(VAR no: No; valor: INTEGER);
  BEGIN
    IF no = NIL THEN
      NEW(no);
      no.valor := valor;
      no.esquerda := NIL;
      no.direita := NIL;
    ELSIF valor < no.valor THEN
      Inserir(no.esquerda, valor);
    ELSE
      Inserir(no.direita, valor);
    END;
  END Inserir;

  PROCEDURE Buscar(no: No; valor: INTEGER): BOOLEAN;
  BEGIN
    IF no = NIL THEN
      RETURN FALSE;
    ELSIF valor = no.valor THEN
      RETURN TRUE;
    ELSIF valor < no.valor THEN
      RETURN Buscar(no.esquerda, valor);
    ELSE
      RETURN Buscar(no.direita, valor);
    END;
  END Buscar;

  PROCEDURE ImprimirEmOrdem(no: No);
  BEGIN
    IF no # NIL THEN
      ImprimirEmOrdem(no.esquerda);
      Write(no.valor, ' ');
      ImprimirEmOrdem(no.direita);
    END;
  END ImprimirEmOrdem;

BEGIN
  raiz := NIL;

  Inserir(raiz, 5);
  Inserir(raiz, 3);
  Inserir(raiz, 8);
  Inserir(raiz, 2);
  Inserir(raiz, 4);
  Inserir(raiz, 7);
  Inserir(raiz, 9);

  IF Buscar(raiz, 7) THEN
    WriteLn("O número 7 está presente na árvore.");
  ELSE
    WriteLn("O número 7 não está presente na árvore.");
  END;

  Write("Em ordem: ");
  ImprimirEmOrdem(raiz);
  WriteLn;
END.
```

Este código implementa uma árvore binária de busca simples em Modula-2. A árvore é representada pelos nós, que possuem um valor inteiro e dois ponteiros para os nós filhos esquerdo e direito. O programa começa definindo o tipo `No` como um ponteiro para `Nodo`, que é a estrutura de dados para cada nó da árvore. Também declara uma variável global `raiz`, que armazena o nó raiz da árvore.

Em seguida, são definidos os procedimentos `Inserir`, `Buscar` e `ImprimirEmOrdem`. O procedimento `Inserir` insere um novo valor na árvore, mantendo a ordem de valores crescente. O procedimento `Buscar` verifica se um valor está presente na árvore. O procedimento `ImprimirEmOrdem` imprime os valores da árvore em ordem crescente.

No trecho principal do código, a árvore é inicializada com alguns valores de exemplo usando o procedimento `Inserir`. Em seguida, é feita uma busca pelo valor 7 e exibida uma mensagem indicando se o valor foi encontrado ou não. Por fim, é chamado o procedimento `ImprimirEmOrdem` para exibir todos os valores da árvore em ordem crescente.