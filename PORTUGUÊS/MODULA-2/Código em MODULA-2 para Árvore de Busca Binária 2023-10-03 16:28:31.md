Claro! Aqui está um código complexo em MODULA-2 que cria uma árvore de busca binária e realiza a busca por um elemento específico na árvore:

```
MODULE ArvoreDeBusca;

FROM InOut IMPORT WriteString, WriteInt, ReadInt, WriteLn;
FROM SYSTEM IMPORT NEW;

TYPE
   NoPtr = POINTER TO No;
   No = RECORD
      valor: INTEGER;
      esquerda, direita: NoPtr;
   END;

VAR
   raiz: NoPtr;

PROCEDURE Inserir(VAR no: NoPtr; valor: INTEGER);
BEGIN
   IF no = NIL THEN
      NEW(no);
      no.valor := valor;
      no.esquerda := NIL;
      no.direita := NIL;
   ELSEIF valor < no.valor THEN
      Inserir(no.esquerda, valor);
   ELSE
      Inserir(no.direita, valor);
   END;
END Inserir;

FUNCTION Buscar(no: NoPtr; valor: INTEGER): BOOLEAN;
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

PROCEDURE Main;
VAR
   num, busca: INTEGER;
BEGIN
   raiz := NIL;

   WriteString("Digite os números para inserir na árvore (digite 0 para encerrar): ");

   REPEAT
      num := ReadInt();

      IF num <> 0 THEN
         Inserir(raiz, num);
      END;
   UNTIL num = 0;

   WriteString("Digite o número a ser buscado: ");
   busca := ReadInt();

   IF Buscar(raiz, busca) THEN
      WriteString("O número está presente na árvore.");
   ELSE
      WriteString("O número não está presente na árvore.");
   END;

   WriteLn;
END Main;

BEGIN
   Main;
END ArvoreDeBusca.
```

Neste código, é criado um módulo chamado "ArvoreDeBusca". O módulo define um tipo de registro chamado "No", que representa um nó da árvore de busca binária. Cada nó possui um valor inteiro, uma referência para o nó filho esquerdo e uma referência para o nó filho direito.

O procedimento "Inserir" recebe um valor inteiro e insere esse valor na árvore de busca binária. Caso o nó atual seja nulo, um novo nó é criado com o valor informado. Caso contrário, o valor é comparado com o valor do nó atual e, dependendo do resultado, a inserção é feita no nó filho esquerdo ou direito, chamando recursivamente o procedimento "Inserir".

A função "Buscar" recebe um valor inteiro e realiza a busca por esse valor na árvore de busca binária. Caso o nó atual seja nulo, significa que o valor não foi encontrado e a função retorna falso. Caso contrário, o valor é comparado com o valor do nó atual e, dependendo do resultado, a busca é realizada no nó filho esquerdo ou direito, chamando recursivamente a função "Buscar".

No procedimento "Main", é criada a raiz da árvore e o usuário pode digitar uma lista de números para inserir na árvore. Quando o número 0 é digitado, a inserção é encerrada. Em seguida, o usuário digita um número para buscar na árvore e a função "Buscar" é chamada. O resultado da busca é exibido na tela.

Este código complexo em MODULA-2 mostra a implementação de uma árvore de busca binária e como realizar a inserção e busca de elementos na árvore.