Claro! Aqui está um código em ADA que abrange várias funcionalidades e utiliza algumas abstrações do paradigma de programação orientada a objetos. O código apresenta uma classe chamada "Pessoa" que possui atributos como nome, idade e uma lista de habilidades. Além disso, a classe também possui métodos para adicionar habilidades à lista, imprimir as informações da pessoa e verificar se ela é maior de idade.

```
with Ada.Text_IO;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Programa_Principal is

   -- Definição da classe Pessoa
   type Pessoa is record
      Nome     : String(1 .. 100);
      Idade    : Integer;
      Habilidades : Vector(Integer range <>) := Empty_Vector;
   end record;

   -- Função para adicionar habilidades à lista
   procedure Adicionar_Habilidade(P : in out Pessoa; Habilidade : Integer) is
   begin
      P.Habilidades.Append(Habilidade);
   end Adicionar_Habilidade;

   -- Procedimento para imprimir as informações da pessoa
   procedure Imprimir_Informacoes(P : in Pessoa) is
   begin
      Ada.Text_IO.Put_Line("Nome: " & P.Nome);
      Ada.Text_IO.Put_Line("Idade: " & Integer'Image(P.Idade));

      -- Verifica se a pessoa tem habilidades cadastradas
      if P.Habilidades.Length > 0 then
         Ada.Text_IO.Put_Line("Habilidades:");

         -- Imprime cada habilidade da pessoa
         for Index in P.Habilidades'Range loop
            Ada.Text_IO.Put_Line(" - " & Integer'Image(P.Habilidades(Index)));
         end loop;
      else
         Ada.Text_IO.Put_Line("Nenhuma habilidade cadastrada.");
      end if;
   end Imprimir_Informacoes;

   -- Função para verificar se a pessoa é maior de idade
   function Maioridade(P : in Pessoa) return Boolean is
   begin
      return P.Idade >= 18;
   end Maioridade;

   -- Programa principal
   P1 : Pessoa;
begin

   -- Preenche os atributos da pessoa
   P1.Nome := "João";
   P1.Idade := 25;

   -- Adiciona algumas habilidades
   Adicionar_Habilidade(P1, 1);
   Adicionar_Habilidade(P1, 2);

   -- Imprime as informações da pessoa
   Imprimir_Informacoes(P1);

   -- Verifica se a pessoa é maior de idade
   if Maioridade(P1) then
      Ada.Text_IO.Put_Line("É maior de idade.");
   else
      Ada.Text_IO.Put_Line("Não é maior de idade.");
   end if;

end Programa_Principal;
```

Nesse código, a classe `Pessoa` é definida com três atributos, sendo eles `Nome`, `Idade` e `Habilidades`. O programa principal cria um objeto do tipo `Pessoa` chamado `P1` e preenche seus atributos com valores de exemplo. Em seguida, algumas habilidades são adicionadas através do método `Adicionar_Habilidade`. O método `Imprimir_Informacoes` imprime na tela todas as informações da pessoa, incluindo o nome, idade e habilidades. Por fim, a função `Maioridade` é utilizada para verificar se a pessoa é maior de idade, e essa informação é exibida no final do programa.