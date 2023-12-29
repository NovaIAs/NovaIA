Claro! Aqui está um código complexo em ADA que envolve a implementação de um algoritmo genético para resolver o problema do caixeiro viajante. O algoritmo busca encontrar a rota mais curta para visitar todas as cidades em um mapa.

```
with Ada.Text_IO;

procedure Problema_Caixeiro_Viajante is

   -- Definição do tipo de vetor
   type Vetor is array (Positive range <>) of Integer;

   -- Definição do tipo de matriz
   type Matriz is array (Positive range <>, Positive range <>) of Integer;

   -- Parâmetros do algoritmo genético
   Tamanho_Populacao : constant := 100;
   Taxa_Crossover    : constant := 0.8;
   Taxa_Mutacao      : constant := 0.2;
   Num_Geracoes      : constant := 1000;

   -- Função para inicializar a população
   function Inicializar_Populacao (Tam : Positive) return Vetor is
      Individuo : Vetor (1 .. Tam);
   begin
      for I in 1 .. Tam loop
         Individuo (I) := I;
      end loop;
      return Individuo;
   end Inicializar_Populacao;

   -- Função para calcular a distância entre duas cidades
   function Distancia (Cidade1, Cidade2 : Integer) return Integer is
   begin
      -- Coloque aqui seu cálculo de distância entre duas cidades
   end Distancia;

   -- Função para calcular o fitness de um indivíduo
   function Fitness (Individuo : Vetor) return Integer is
      Total_Distancia : Integer := 0;
   begin
      for I in 1 .. Individuo'Length - 1 loop
         Total_Distancia := Total_Distancia + Distancia (Individuo (I), Individuo (I + 1));
      end loop;
      Total_Distancia := Total_Distancia + Distancia (Individuo (Individuo'Last), Individuo (1));
      return Total_Distancia;
   end Fitness;

   -- Função para realizar o crossover entre dois indivíduos
   function Crossover (Pai1, Pai2 : Vetor) return Vetor is
      Filho : Vetor (1 .. Pai1'Length);
      Ponto_Corte : Integer := Pai1'Length / 2;
   begin
      Filho := (others => 0);
      for I in 1 .. Ponto_Corte loop
         Filho (I) := Pai1 (I);
      end loop;
      for I in Ponto_Corte + 1 .. Pai2'Length loop
         for J in 1 .. Filho'Length loop
            if not (Pai2 (I) in Filho) then
               Filho (J) := Pai2 (I);
               exit;
            end if;
         end loop;
      end loop;
      return Filho;
   end Crossover;

   -- Função para realizar a mutação em um indivíduo
   procedure Mutacao (Individuo : in out Vetor) is
      Pos1, Pos2 : Integer;
      Aux : Integer;
   begin
      Pos1 := Positive'Random (Individuo'Length) + 1;
      Pos2 := Positive'Random (Individuo'Length) + 1;
      Aux := Individuo (Pos1);
      Individuo (Pos1) := Individuo (Pos2);
      Individuo (Pos2) := Aux;
   end Mutacao;

   -- Função para selecionar dois pais para reprodução
   procedure Selecionar_Pais (Populacao : in Vetor; out Pai1, Pai2 : out Vetor) is
      Individuo1, Individuo2 : Integer;
   begin
      Individuo1 := Positive'Random (Populacao'Length) + 1;
      Individuo2 := Positive'Random (Populacao'Length) + 1;
      Pai1 := Populacao (Individuo1);
      Pai2 := Populacao (Individuo2);
   end Selecionar_Pais;

   -- Função para atualizar a população com os descendentes
   procedure Atualizar_Populacao (Populacao : in out Vetor; Descendentes : in Vetor) is
   begin
      Populacao := Descendentes;
   end Atualizar_Populacao;

   -- Função principal que resolve o problema do caixeiro viajante
   procedure Resolver_Problema is
      Populacao : Vetor (1 .. Tamanho_Populacao);
      Descendentes : Vetor (1 .. Tamanho_Populacao);
      Melhor_Individuo : Vetor (1 .. Populacao'Length);
      Melhor_Fitness : Integer := Integer'Last;
      Geracoes : Integer := 0;
      Pai1, Pai2 : Vetor (1 .. Populacao'Length);
   begin
      -- Inicialização da população
      Populacao := Inicializar_Populacao (Populacao'Length);
      
      -- Avaliação inicial da população
      for I in 1 .. Populacao'Length loop
         if Fitness (Populacao (I)) < Melhor_Fitness then
            Melhor_Fitness := Fitness (Populacao (I));
            Melhor_Individuo := Populacao (I);
         end if;
      end loop;
      
      -- Loop principal do algoritmo genético
      while Geracoes < Num_Geracoes loop
         Geracoes := Geracoes + 1;
         
         for I in 1 .. Populacao'Length loop
            -- Seleção de pais
            Selecionar_Pais (Populacao, Pai1, Pai2);
            
            -- Realização do crossover
            if Float (Positive'Random (100)) / 100.0 <= Taxa_Crossover then
               Descendentes (I) := Crossover (Pai1, Pai2);
            else
               Descendentes (I) := Pai1;
            end if;
            
            -- Realização da mutação
            if Float (Positive'Random (100)) / 100.0 <= Taxa_Mutacao then
               Mutacao (Descendentes (I));
            end if;
         end loop;
         
         -- Atualização da população
         Atualizar_Populacao (Populacao, Descendentes);
         
         -- Avaliação da nova população
         for I in 1 .. Populacao'Length loop
            if Fitness (Populacao (I)) < Melhor_Fitness then
               Melhor_Fitness := Fitness (Populacao (I));
               Melhor_Individuo := Populacao (I);
            end if;
         end loop;
      end loop;
      
      -- Exibição do resultado
      Ada.Text_IO.Put_Line ("Melhor rota encontrada: " & Melhor_Individuo'Image);
      Ada.Text_IO.Put_Line ("Distância percorrida: " & Melhor_Fitness'Image);
   end Resolver_Problema;

begin
   Resolver_Problema;
end Problema_Caixeiro_Viajante;
```

Esse código implementa um algoritmo genético para resolver o problema do caixeiro viajante. Ele utiliza uma população de indivíduos representados por vetores numéricos, onde cada elemento representa uma cidade a ser visitada. O algoritmo evolui a população ao longo de um número fixo de gerações, aplicando operações de crossover e mutação para gerar descendentes.

A função `Inicializar_Populacao` cria uma população inicial com os indivíduos representando todas as cidades. A função `Distancia` calcula a distância entre duas cidades, que deve ser implementada pelo usuário de acordo com a lógica desejada. A função `Fitness` calcula a distância total percorrida por um indivíduo, somando as distâncias entre as cidades em sua rota.

As funções `Crossover` e `Mutacao` são responsáveis por realizar as operações de crossover e mutação, respectivamente. O ponto de corte para o crossover é definido na variável `Ponto_Corte`, que divide a rota do pai 1 e do pai 2.

A função `Selecionar_Pais` seleciona dois pais aleatórios da população atual. A função `Atualizar_Populacao` atualiza a população com os descendentes gerados.

A função principal `Resolver_Problema` inicializa a população, avalia a população inicial, executa o loop principal do algoritmo genético, atualiza a população e avalia novamente. No final, exibe a melhor rota encontrada e a distância percorrida.

Espero que este código atenda às suas expectativas!