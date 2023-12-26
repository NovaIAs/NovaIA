Claro! Aqui está um código complexo em ADA, criado exclusivamente para você:

```
-- Este é um programa em ADA que implementa uma árvore binária de busca avançada (AVL).
-- A AVL é uma estrutura de dados que mantém o balanceamento automático da árvore para garantir a eficiência das operações de busca, inserção e exclusão.

with Ada.Text_IO; use Ada.Text_IO;

procedure AVL_Tree is
   type Node is record
      Value  : Integer;
      Height : Integer;
      Left   : access Node;
      Right  : access Node;
   end record;

   type Tree is access Node;

   -- Função que retorna a altura de um nó
   function Get_Height(N : Tree) return Integer is
   begin
      if N = null then
         return -1;
      else
         return N.Height;
      end if;
   end Get_Height;

   -- Função que retorna o máximo entre dois números
   function Max(A, B : Integer) return Integer is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;

   -- Função que realiza a rotação simples para a esquerda
   function Rotate_Left(K2 : in out Tree) return Tree is
      K1 : Tree := K2.Right;
   begin
      K2.Right := K1.Left;
      K1.Left := K2;
      K2.Height := Max(Get_Height(K2.Left), Get_Height(K2.Right)) + 1;
      K1.Height := Max(Get_Height(K1.Right), K2.Height) + 1;
      return K1;
   end Rotate_Left;

   -- Função que realiza a rotação simples para a direita
   function Rotate_Right(K1 : in out Tree) return Tree is
      K2 : Tree := K1.Left;
   begin
      K1.Left := K2.Right;
      K2.Right := K1;
      K1.Height := Max(Get_Height(K1.Left), Get_Height(K1.Right)) + 1;
      K2.Height := Max(Get_Height(K2.Left), K1.Height) + 1;
      return K2;
   end Rotate_Right;

   -- Função que realiza a rotação dupla direita-esquerda
   function Double_Rotate_Right_Left(K3 : in out Tree) return Tree is
   begin
      K3.Right := Rotate_Right(K3.Right);
      return Rotate_Left(K3);
   end Double_Rotate_Right_Left;

   -- Função que realiza a rotação dupla esquerda-direita
   function Double_Rotate_Left_Right(K1 : in out Tree) return Tree is
   begin
      K1.Left := Rotate_Left(K1.Left);
      return Rotate_Right(K1);
   end Double_Rotate_Left_Right;

   -- Função que insere um elemento na árvore AVL
   function Insert_Item(Item : Integer; T : in out Tree) return Tree is
   begin
      if T = null then
         T := new Node'(Value => Item, Height => 0, Left => null, Right => null);
      elsif Item < T.Value then
         T.Left := Insert_Item(Item, T.Left);
         if Get_Height(T.Left) - Get_Height(T.Right) = 2 then
            if Item < T.Left.Value then
               T := Rotate_Right(T);
            else
               T := Double_Rotate_Left_Right(T);
            end if;
         end if;
      elsif Item > T.Value then
         T.Right := Insert_Item(Item, T.Right);
         if Get_Height(T.Right) - Get_Height(T.Left) = 2 then
            if Item > T.Right.Value then
               T := Rotate_Left(T);
            else
               T := Double_Rotate_Right_Left(T);
            end if;
         end if;
      else
         Put_Line("O valor " & Item'Image & " já existe na árvore.");
      end if;

      T.Height := Max(Get_Height(T.Left), Get_Height(T.Right)) + 1;
      return T;
   end Insert_Item;

   -- Função que imprime a árvore em ordem crescente
   procedure Print_In_Order(T : Tree) is
      procedure Print(T : Tree) is
      begin
         if T /= null then
            Print(T.Left);
            Put(T.Value'Image & " ");
            Print(T.Right);
         end if;
      end Print;
   begin
      Print(T);
      New_Line;
   end Print_In_Order;

   -- Programa principal
   T : Tree := null;
begin
   -- Inserindo elementos na árvore AVL
   T := Insert_Item(10, T);
   T := Insert_Item(20, T);
   T := Insert_Item(30, T);
   T := Insert_Item(40, T);
   T := Insert_Item(50, T);
   T := Insert_Item(25, T);

   -- Imprimindo a árvore em ordem crescente
   Put_Line("Árvore AVL em ordem crescente:");
   Print_In_Order(T);
end AVL_Tree;
```

Este código em ADA implementa uma árvore binária de busca avançada (AVL). A AVL é uma estrutura de dados que mantém o balanceamento automático da árvore para garantir a eficiência das operações de busca, inserção e exclusão.

O código começa definindo um tipo de registro chamado `Node`, que representa um nó da árvore. Cada nó contém um valor, uma altura, uma referência para o nó à esquerda e uma referência para o nó à direita.

Em seguida, é definido um tipo de acesso chamado `Tree`, que é um ponteiro para um nó da árvore.

O código também define uma série de funções auxiliares. A função `Get_Height` retorna a altura de um nó. A função `Max` retorna o máximo entre dois números. As funções `Rotate_Left`, `Rotate_Right`, `Double_Rotate_Right_Left` e `Double_Rotate_Left_Right` são responsáveis por realizar as rotações necessárias para manter o balanceamento da árvore. A função `Insert_Item` insere um novo elemento na árvore AVL, garantindo que o balanceamento seja mantido. E por fim, a função `Print_In_Order` imprime os elementos da árvore em ordem crescente.

No programa principal, são inseridos alguns elementos na árvore AVL usando a função `Insert_Item`. Em seguida, a função `Print_In_Order` é chamada para imprimir os elementos da árvore em ordem crescente.

Espero que este código atenda às suas expectativas!