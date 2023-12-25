Claro! Aqui está um código complexo em ADA:

```
-- Este é um programa em ADA que implementa um algoritmo de busca em largura em um grafo.
-- O grafo é representado como uma matriz de adjacência.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Busca_Em_Largura is

   -- Definindo o tamanho máximo do grafo
   Tamanho_Máximo : constant := 10;

   -- Definindo o grafo como uma matriz de adjacência
   type Grafo is array(1..Tamanho_Máximo, 1..Tamanho_Máximo) of Boolean;

   -- Inicializando o grafo com todos os valores falsos
   Grafo_Vazio : constant Grafo := (others => (others => False));

   -- Definindo uma fila para armazenar os vértices a serem visitados
   type Fila is record
      Elementos : Ada.Strings.Unbounded.Unbounded_String;
      Tamanho : Natural := 0;
   end record;

   -- Função para adicionar um elemento à fila
   procedure Enfileirar(Item : in Character; F : in out Fila) is begin
      Append(F.Elementos, Character'Val(Item));
      F.Tamanho := F.Tamanho + 1;
   end Enfileirar;

   -- Função para remover um elemento da fila
   function Desenfileirar(F : in out Fila) return Character is
      Resultado : Character := F.Elementos(F.Tamanho);
   begin
      F.Elementos := F.Elementos(1..F.Tamanho-1);
      F.Tamanho := F.Tamanho - 1;
      return Resultado;
   end Desenfileirar;

   -- Função para verificar se a fila está vazia
   function Fila_Vazia(F : Fila) return Boolean is
   begin
      return F.Tamanho = 0;
   end Fila_Vazia;

   -- Procedimento principal para a busca em largura
   procedure Busca_Largura(G : in Grafo; Inicio : in Natural) is

      Visitado : array(1..Tamanho_Máximo) of Boolean := (others => False);
      F : Fila;
      V : Natural;

   begin
      -- Enfileirando o vértice inicial
      Enfileirar(Character'Val(Inicio), F);
      Visitado(Inicio) := True;

      -- Enquanto a fila não estiver vazia
      while not Fila_Vazia(F) loop
         -- Desenfileirando um vértice
         V := Natural(Desenfileirar(F));

         -- Imprimindo o vértice visitado
         Put("Visitando o vértice ");
         Put(V, Width => 3);
         New_Line;

         -- Percorrendo todos os vértices adjacentes a V
         for Adjacente in 1..Tamanho_Máximo loop
            -- Se o vértice Adjacente ainda não foi visitado e é adjacente a V
            if not Visitado(Adjacente) and G(V, Adjacente) then
               -- Enfileirando o vértice Adjacente
               Enfileirar(Character'Val(Adjacente), F);
               Visitado(Adjacente) := True;
            end if;
         end loop;
      end loop;

   end Busca_Largura;

   -- Função auxiliar para imprimir o grafo
   procedure Imprimir_Grafo(G : in Grafo) is
   begin
      for I in 1..Tamanho_Máximo loop
         for J in 1..Tamanho_Máximo loop
            Put(G(I, J), Width => 5);
         end loop;
         New_Line;
      end loop;
   end Imprimir_Grafo;

   -- Definindo o grafo de exemplo
   Grafo_Exemplo : constant Grafo :=
      ((False, True, False, False, False, True, False, False, False, False),
       (True, False, True, False, False, False, True, False, False, False),
       (False, True, False, True, True, False, False, False, False, False),
       (False, False, True, False, False, False, False, False, True, False),
       (False, False, True, False, False, False, False, True, False, False),
       (True, False, False, False, False, False, False, False, False, True),
       (False, True, False, False, False, False, False, False, False, False),
       (False, False, False, False, True, False, False, False, False, False),
       (False, False, False, True, False, False, False, False, False, True),
       (False, False, False, False, False, True, False, False, True, False));

begin
   -- Imprimindo o grafo de exemplo
   Put("Grafo de exemplo:");
   New_Line;
   Imprimir_Grafo(Grafo_Exemplo);
   New_Line;

   -- Realizando a busca em largura no grafo de exemplo, começando pelo vértice 1
   Busca_Largura(Grafo_Exemplo, 1);
   New_Line;

end Busca_Em_Largura;
```

Este código implementa um algoritmo de busca em largura em um grafo representado como uma matriz de adjacência. Ele começa definindo o tamanho máximo do grafo e a matriz de adjacência. Em seguida, define uma estrutura de fila para auxiliar na busca em largura.

O procedimento principal `Busca_Largura` realiza a busca em largura no grafo. Ele começa enfileirando o vértice inicial e marcando-o como visitado. Em seguida, enquanto a fila não estiver vazia, ele desenfileira um vértice, o visita e enfileira todos os vértices adjacentes a ele que ainda não foram visitados.

O procedimento `Imprimir_Grafo` é uma função auxiliar que imprime a matriz de adjacência do grafo.

No final do programa, é definido um grafo de exemplo e é realizada a busca em largura começando pelo vértice 1.

Espero que este código atenda às suas expectativas!