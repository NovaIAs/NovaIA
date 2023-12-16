Claro! Abaixo está um código em ADA que implementa um algoritmo de busca em largura (BFS) em um grafo não direcionado. O código é escrito em português e possui comentários explicativos para facilitar a compreensão:

```
-- Algoritmo de Busca em Largura (BFS) em um Grafo Não Direcionado

with Ada.Text_IO;
use Ada.Text_IO;

procedure Busca_Em_Largura is

   -- Definição de um tipo para representar os nós do grafo
   type No is
      record
         Visitado: Boolean;  -- Indica se o nó já foi visitado
         -- Outros atributos do nó (caso necessário)
      end record;

   -- Definição de um tipo para representar um grafo não direcionado
   type Grafo is
      array (Positive range <>) of No;
      -- Outros atributos do grafo (caso necessário)

   procedure BFS (G: in Grafo; NoInicial: in Positive) is
      -- Queue para armazenar os nós a serem visitados
      type Fila is record
         Elementos: array (Positive range <>) of Positive;
         Inicio: Positive;
         Fim: Positive;
      end record;
      F: Fila;

      -- Inicialização do grafo e da fila
      procedure Inicializar_Grafo (G: out Grafo) is
      begin
         for I in G'range loop
            G(I).Visitado := False;
         end loop;
      end Inicializar_Grafo;

      procedure Inicializar_Fila (F: out Fila; Tamanho: Positive) is
      begin
         F.Elementos := (others => 0);
         F.Inicio := 0;
         F.Fim := 0;
      end Inicializar_Fila;

      -- Adiciona um nó na fila
      procedure Enfileirar (F: in out Fila; No: in Positive) is
      begin
         if F.Fim < F.Elementos'Last then
            F.Fim := F.Fim + 1;
            F.Elementos(F.Fim) := No;
         end if;
      end Enfileirar;

      -- Remove um nó da fila
      procedure Desenfileirar (F: in out Fila; No: out Positive) is
      begin
         if F.Inicio < F.Fim then
            F.Inicio := F.Inicio + 1;
            No := F.Elementos(F.Inicio);
         end if;
      end Desenfileirar;

      -- Verifica se a fila está vazia
      function Fila_Vazia (F: in Fila) return Boolean is
      begin
         return F.Inicio = F.Fim;
      end Fila_Vazia;

   begin
      Inicializar_Grafo(G);
      
      -- Marca o nó inicial como visitado
      G(NoInicial).Visitado := True;

      -- Inicializa a fila e adiciona o nó inicial
      Inicializar_Fila(F, G'Length);
      Enfileirar(F, NoInicial);

      -- Percorre todos os nós da fila
      while not Fila_Vazia(F) loop
         -- Remove o nó da fila
         Desenfileirar(F, NoAtual);

         -- Processa o nó atual (pode ser exibido, armazenado, etc.)
         Put_Line("Visitando nó: " & Positive'Image(NoAtual));

         -- Percorre todos os nós adjacentes ao nó atual
         for I in G'range loop
            -- Verifica se o nó adjacente ainda não foi visitado
            if not G(I).Visitado then
               -- Marca o nó adjacente como visitado
               G(I).Visitado := True;

               -- Adiciona o nó adjacente na fila
               Enfileirar(F, I);
            end if;
         end loop;
      end loop;
   end BFS;

   -- Exemplo de uso do algoritmo
   G: Grafo(1..6);

begin
   -- Inicializa o grafo com 6 nós
   G := (1 => (Visitado => False),  -- Nó 1
         2 => (Visitado => False),  -- Nó 2
         3 => (Visitado => False),  -- Nó 3
         4 => (Visitado => False),  -- Nó 4
         5 => (Visitado => False),  -- Nó 5
         6 => (Visitado => False)); -- Nó 6

   -- Definição das arestas do grafo
   -- Exemplo: nó 1 é adjacente aos nós 2, 3 e 4
   --         nó 2 é adjacente aos nós 1, 5 e 6
   --         e assim por diante
   G(1).Adjacentes := (2, 3, 4);
   G(2).Adjacentes := (1, 5, 6);
   G(3).Adjacentes := (1);
   G(4).Adjacentes := (1);
   G(5).Adjacentes := (2);
   G(6).Adjacentes := (2);

   -- Chama o algoritmo BFS, iniciando pelo nó 1
   BFS(G, 1);

end Busca_Em_Largura;
```

Esse código implementa o algoritmo de busca em largura (BFS) em um grafo não direcionado. Ele utiliza tipos de dados personalizados para representar os nós e o grafo, e também inclui uma fila para armazenar os nós a serem visitados. O algoritmo começa marcando o nó inicial como visitado e adicionando-o à fila. Em seguida, ele percorre todos os nós da fila, removendo um nó por vez e processando-o. Para cada nó removido, o algoritmo verifica todos os nós adjacentes que ainda não foram visitados, marca-os como visitados e os adiciona à fila. O processo continua até que a fila esteja vazia.

Espero que este código atenda às suas expectativas! Se você tiver outras solicitações ou precisar de mais informações, fique à vontade para perguntar.