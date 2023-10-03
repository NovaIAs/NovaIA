Claro! Aqui está um exemplo de um código complexo em ADA, que é uma linguagem de programação estruturada baseada em Pascal. Este código é um sistema de gerenciamento de funcionários de uma empresa, que permite adicionar, editar e excluir funcionários, além de calcular a folha de pagamento mensal. 

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Gerenciamento_Funcionarios is

   -- Declaração de tipos e estruturas de dados

   type Nome is array (1..50) of Character;
   type Endereco is array (1..100) of Character;
   type Data is record
      Dia   : Integer range 1..31;
      Mes   : Integer range 1..12;
      Ano   : Integer range 1900..2100;
   end record;

   type Funcionario is record
      ID         : Integer;
      Nome       : Nome;
      Endereco   : Endereco;
      DataNasc   : Data;
      Salario    : Float;
   end record;

   type Lista_Funcionarios is array (1..100) of Funcionario;
   Lista : Lista_Funcionarios;
   Contador : Integer := 0;

   -- Função para adicionar novo funcionário
   
   procedure Adicionar_Funcionario (ID : in Integer; Nome : in Nome;
                                   Endereco : in Endereco; DataNasc : in Data;
                                   Salario : in Float) is
   begin
      Contador := Contador + 1;
      Lista(Contador).ID := ID;
      Lista(Contador).Nome := Nome;
      Lista(Contador).Endereco := Endereco;
      Lista(Contador).DataNasc := DataNasc;
      Lista(Contador).Salario := Salario;
   end Adicionar_Funcionario;

   -- Função para editar os dados de um funcionário existente

   procedure Editar_Funcionario (ID : in Integer; Nome : in Nome;
                                Endereco : in Endereco; DataNasc : in Data;
                                Salario : in Float) is
      Encontrado : Boolean := False;
   begin
      for I in 1..Contador loop
         if Lista(I).ID = ID then
            Lista(I).Nome := Nome;
            Lista(I).Endereco := Endereco;
            Lista(I).DataNasc := DataNasc;
            Lista(I).Salario := Salario;
            Encontrado := True;
         end if;
      end loop;

      if not Encontrado then
         Put_Line("Funcionário não encontrado.");
      end if;
   end Editar_Funcionario;

   -- Função para excluir um funcionário existente

   procedure Excluir_Funcionario (ID : in Integer) is
      Encontrado : Boolean := False;
   begin
      for I in 1..Contador loop
         if Lista(I).ID = ID then
            for J in I..Contador-1 loop
               Lista(J) := Lista(J+1);
            end loop;
            Contador := Contador - 1;
            Encontrado := True;
         end if;
      end loop;

      if not Encontrado then
         Put_Line("Funcionário não encontrado.");
      end if;
   end Excluir_Funcionario;

   -- Função para calcular a folha de pagamento mensal

   procedure Calcular_Folha_Pagamento is
      Total_Salarios : Float := 0.0;
   begin
      for I in 1..Contador loop
         Total_Salarios := Total_Salarios + Lista(I).Salario;
      end loop;

      Put_Line("Total de salários a pagar este mês: " & Float'Image(Total_Salarios));
   end Calcular_Folha_Pagamento;

begin
   -- Exemplo de uso das funções
   
   Adicionar_Funcionario(1, "João Silva", "Rua A, 123", (10, 12, 1980), 2500.0);
   Adicionar_Funcionario(2, "Maria Santos", "Avenida B, 456", (5, 5, 1990), 3000.0);
   Editar_Funcionario(2, "Maria Oliveira", "Avenida B, 789", (5, 5, 1990), 3200.0);
   Excluir_Funcionario(1);
   Calcular_Folha_Pagamento;
   
   -- Exemplo de impressão dos dados dos funcionários
   
   for I in 1..Contador loop
      Put_Line("ID: " & Integer'Image(Lista(I).ID));
      Put_Line("Nome: " & Lista(I).Nome);
      Put_Line("Endereço: " & Lista(I).Endereco);
      Put_Line("Data de Nascimento: " & Integer'Image(Lista(I).DataNasc.Dia) & "/" &
               Integer'Image(Lista(I).DataNasc.Mes) & "/" &
               Integer'Image(Lista(I).DataNasc.Ano));
      Put_Line("Salário: " & Float'Image(Lista(I).Salario));
      New_Line;
   end loop;

end Gerenciamento_Funcionarios;
```

Neste código, temos a declaração de tipos de dados personalizados para representar informações dos funcionários, como nome, endereço e data de nascimento. Em seguida, temos a definição de uma estrutura de dados para armazenar os funcionários em uma lista. 

A seguir, temos as funções `Adicionar_Funcionario`, `Editar_Funcionario` e `Excluir_Funcionario`, que permitem adicionar, editar e excluir funcionários da lista, respectivamente. Essas funções são implementadas usando loops e condicionais para percorrer a lista e encontrar o funcionário desejado.

Por fim, temos a função `Calcular_Folha_Pagamento`, que realiza a soma de todos os salários da lista de funcionários e imprime o total a ser pago no mês. 

O código inclui um exemplo de uso dessas funções, adicionando dois funcionários, editando o segundo, excluindo o primeiro e, em seguida, calculando a folha de pagamento mensal. Em seguida, os dados dos funcionários restantes são impressos na tela.

Esse é apenas um exemplo de código complexo em ADA, mostrando o uso de tipos de dados personalizados, estruturas de controle e manipulação de listas. A linguagem ADA é conhecida por sua ênfase em segurança e confiabilidade, sendo amplamente utilizada em aplicações críticas, como sistemas embarcados, aviação e saúde.