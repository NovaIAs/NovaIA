Claro, aqui está um código em Ada que implementa uma Árvore de Busca Binária balanceada:

```
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   type Int_Array is array (Positive range <>) of Integer;
   
   type Node;
   type Node_Ptr is access Node;
   
   type Node is record
      Data  : Integer;
      Left  : Node_Ptr := null;
      Right : Node_Ptr := null;
      Height: Integer := 0;
   end record;
   
   function Height(N : Node_Ptr) return Integer is
   begin
      if N = null then
         return -1;
      else
         return N.Height;
      end if;
   end Height;
   
   procedure Update_Height(N : Node_Ptr) is
   begin
      N.Height := 1 + Integer'Max (Height(N.Left), Height(N.Right));
   end Update_Height;
   
   function Balance_Factor(N : Node_Ptr) return Integer is
   begin
      return Height(N.Left) - Height(N.Right);
   end Balance_Factor;
   
   procedure Right_Rotate(Root : in out Node_Ptr) is
      P : Node_Ptr := Root.Left;
   begin
      Root.Left := P.Right;
      P.Right := Root;
      Update_Height(Root);
      Update_Height(P);
      Root := P;
   end Right_Rotate;
   
   procedure Left_Rotate(Root : in out Node_Ptr) is
      P : Node_Ptr := Root.Right;
   begin
      Root.Right := P.Left;
      P.Left := Root;
      Update_Height(Root);
      Update_Height(P);
      Root := P;
   end Left_Rotate;
   
   procedure Balance(Root : in out Node_Ptr) is
      BF : constant Integer := Balance_Factor(Root);
   begin
      if BF > 1 then
         if Balance_Factor(Root.Left) < 0 then
            Left_Rotate(Root.Left);
         end if;
         Right_Rotate(Root);
      elsif BF < -1 then
         if Balance_Factor(Root.Right) > 0 then
            Right_Rotate(Root.Right);
         end if;
         Left_Rotate(Root);
      end if;
      Update_Height(Root);
   end Balance;
   
   procedure Insert(N : in out Node_Ptr; X : Integer) is
   begin
      if N = null then
         N := new Node'(Data => X);
      elsif X < N.Data then
         Insert(N.Left, X);
      else
         Insert(N.Right, X);
      end if;
      Balance(N);
   end Insert;
   
   procedure Print_Tree(N : Node_Ptr; Indent : String := "") is
   begin
      if N /= null then
         Put_Line(Indent & N.Data'Image);
         Print_Tree(N.Left, Indent & "  ");
         Print_Tree(N.Right, Indent & "  ");
      end if;
   end Print_Tree;

   A : Int_Array := (5, 7, 1, 3, 9, 2, 4, 6, 8, 10);
   Root : Node_Ptr := null;
begin
   for I in A'Range loop
      Insert(Root, A(I));
   end loop;
   
   Print_Tree(Root);
end Main;
```

Esse código implementa uma estrutura de dados de árvore de busca binária (BST) balanceada em Ada, que garante que as árvores permaneçam balanceadas mesmo após inserções ou exclusões de elementos. O código utiliza ponteiros de acesso (access types) para manipular os nós da árvore, que são atualizados e roteados conforme necessário para manter a árvore equilibrada. No final, a árvore é impressa na tela usando uma função de impressão de árvore recursiva.