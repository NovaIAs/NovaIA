Claro! Aqui está um exemplo de um código complexo em ADA que utiliza uma estrutura de dados chamada Árvore Binária de Busca para armazenar e organizar um conjunto de números inteiros. O código foi escrito em português e contém comentários explicativos para facilitar a compreensão:

```ada
with Ada.Text_IO; use Ada.Text_IO;

-- Definindo um tipo de dado para representar um nó na árvore
package ArvoreBinaria is
   type No is private;
   type No_Acesso is access No;
   
   -- Função para criar um novo nó
   function Criar_No(Valor : Integer) return No_Acesso;
   
   -- Procedimento para inserir um valor em uma árvore
   procedure Inserir(Valor : Integer; Raiz : in out No);
   
   -- Procedimento para imprimir a árvore em ordem crescente
   procedure Imprimir_Em_Ordem(Raiz : No);
   
private
   type No is record
      Esquerda, Direita : No_Acesso := null;
      Valor : Integer;
   end record;
end ArvoreBinaria;

package body ArvoreBinaria is
   function Criar_No(Valor : Integer) return No_Acesso is
      Novo_No : No_Acesso := new No;
   begin
      Novo_No.Valor := Valor;
      return Novo_No;
   end Criar_No;
   
   procedure Inserir(Valor : Integer; Raiz : in out No) is
   begin
      if Raiz = null then
         Raiz := Criar_No(Valor);
      else
         if Valor < Raiz.Valor then
            Inserir(Valor, Raiz.Esquerda);
         else
            Inserir(Valor, Raiz.Direita);
         end if;
      end if;
   end Inserir;
   
   procedure Imprimir_Em_Ordem(Raiz : No) is
   begin
      if Raiz /= null then
         Imprimir_Em_Ordem(Raiz.Esquerda);
         Put(Raiz.Valor'Image);
         Put(" ");
         Imprimir_Em_Ordem(Raiz.Direita);
      end if;
   end Imprimir_Em_Ordem;
end ArvoreBinaria;

with ArvoreBinaria; use ArvoreBinaria;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Teste_Arvore is
   Raiz : No_Acesso := null;
   Valor : Integer;
   Opcao : Character;
begin
   loop
      Put("Digite um número inteiro para adicionar na árvore: ");
      Get(Valor);
      Inserir(Valor, Raiz);
      
      Put("Deseja adicionar mais um número? (S/N): ");
      Get(Opcao);
      
      exit when Opcao = 'N' or Opcao = 'n';
   end loop;
   
   Put_Line("Árvore em ordem crescente: ");
   Imprimir_Em_Ordem(Raiz);
end Teste_Arvore;

begin
   Teste_Arvore;
end.
```

Este código em ADA implementa uma Árvore Binária de Busca, uma estrutura de dados que permite armazenar e organizar um conjunto de números inteiros de forma eficiente. A árvore é construída inserindo-se valores um a um e, em seguida, imprime os valores em ordem crescente.

A estrutura da árvore é definida no pacote `ArvoreBinaria`, que possui um tipo de dado `No` que representa um nó na árvore. A árvore é composta por nós que possuem um valor inteiro e dois ponteiros para os nós filhos esquerdo e direito.

O pacote `ArvoreBinaria` também define as operações de criação de um novo nó (`Criar_No`), inserção de um valor na árvore (`Inserir`) e impressão da árvore em ordem crescente (`Imprimir_Em_Ordem`).

O programa principal (`Teste_Arvore`) utiliza o pacote `ArvoreBinaria` para criar uma árvore vazia (`Raiz := null`), solicita ao usuário que digite números inteiros para adicionar na árvore e, em seguida, imprime os valores em ordem crescente.

Espero que este código em ADA complexo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição para ajudar.