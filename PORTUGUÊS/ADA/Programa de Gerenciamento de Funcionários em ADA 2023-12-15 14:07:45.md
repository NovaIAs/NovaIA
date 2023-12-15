Claro! Aqui está um exemplo de código em ADA que cria um programa de gerenciamento de funcionários em uma empresa hipotética. O programa permite adicionar, remover, editar e exibir informações dos funcionários.

```ada
with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Gerenciamento_Funcionarios is
   type Nome is new Unbounded_String;

   type Funcionario is record
      Id        : Integer;
      Nome      : Nome;
      Cargo     : Nome;
      Departamento   : Nome;
      Salario   : Float;
   end record;

   type Lista_Funcionarios is array (Positive range <>) of Funcionario;
   type Ponteiro_Funcionarios is access all Lista_Funcionarios;

   function Criar_Funcionario(Id: Integer; Nome: Nome; Cargo: Nome; Departamento: Nome; Salario: Float) return Funcionario is
      Novo_Funcionario: Funcionario;
   begin
      Novo_Funcionario.Id := Id;
      Novo_Funcionario.Nome := Nome;
      Novo_Funcionario.Cargo := Cargo;
      Novo_Funcionario.Departamento := Departamento;
      Novo_Funcionario.Salario := Salario;

      return Novo_Funcionario;
   end Criar_Funcionario;

   procedure Adicionar_Funcionario(Lista: in out Ponteiro_Funcionarios; Novo_Funcionario: Funcionario) is
      Novo_Tamanho: constant Positive := Lista'Length + 1;
   begin
      if Lista = null then
         Lista := new Lista_Funcionarios(1..Novo_Tamanho);
         Lista(Novo_Tamanho) := Novo_Funcionario;
      else
         Lista := Lista_Funcionarios'(Lista(1..Lista'Length), Novo_Funcionario);
      end if;

      Put_Line("Funcionário adicionado: " & Funcionario_To_String(Novo_Funcionario));
   end Adicionar_Funcionario;

   procedure Remover_Funcionario(Lista: in out Ponteiro_Funcionarios; Id: Integer) is
   begin
      if Lista = null or else Lista'Length = 0 then
         Put_Line("Lista de funcionários vazia.");
      else
         declare
            Indice: constant Positive := Encontrar_Indice_Funcionario(Lista, Id);
         begin
            if Indice > 0 then
               Put_Line("Funcionário removido: " & Funcionario_To_String(Lista(Indice)));
               Lista := Lista_Funcionarios'(Lista(1..Indice-1) & Lista(Indice+1..Lista'Length));
            else
               Put_Line("Funcionário não encontrado.");
            end if;
         end;
      end if;
   end Remover_Funcionario;

   procedure Editar_Funcionario(Lista: in out Ponteiro_Funcionarios; Id: Integer; Novo_Funcionario: Funcionario) is
   begin
      if Lista = null or else Lista'Length = 0 then
         Put_Line("Lista de funcionários vazia.");
      else
         declare
            Indice: constant Positive := Encontrar_Indice_Funcionario(Lista, Id);
         begin
            if Indice > 0 then
               Put_Line("Funcionário editado: " & Funcionario_To_String(Lista(Indice)));
               Lista(Indice) := Novo_Funcionario;
            else
               Put_Line("Funcionário não encontrado.");
            end if;
         end;
      end if;
   end Editar_Funcionario;

   procedure Exibir_Funcionarios(Lista: Ponteiro_Funcionarios) is
   begin
      if Lista = null or else Lista'Length = 0 then
         Put_Line("Lista de funcionários vazia.");
      else
         for I in 1..Lista'Length loop
            Put_Line(Funcionario_To_String(Lista(I)));
         end loop;
      end if;
   end Exibir_Funcionarios;

   function Funcionario_To_String(F: Funcionario) return String is
      Resultado: String(1..255);
   begin
      Resultado := "Id: " & Integer'Image(F.Id) &
                   ", Nome: " & To_String(F.Nome) &
                   ", Cargo: " & To_String(F.Cargo) &
                   ", Departamento: " & To_String(F.Departamento) &
                   ", Salário: " & Float'Image(F.Salario);

      return Resultado;
   end Funcionario_To_String;

   function Encontrar_Indice_Funcionario(Lista: Ponteiro_Funcionarios; Id: Integer) return Positive is
   begin
      if Lista /= null and then Lista'Length > 0 then
         for I in 1..Lista'Length loop
            if Lista(I).Id = Id then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Encontrar_Indice_Funcionario;

   Lista_Func: Ponteiro_Funcionarios := null;
   Opcao: Character;
   Id, Novo_Id: Integer;
   Novo_Nome, Novo_Cargo, Novo_Departamento: Nome;
   Novo_Salario: Float;

begin
   loop
      Put_Line("Selecione a opção:");
      Put_Line("1. Adicionar funcionário");
      Put_Line("2. Remover funcionário");
      Put_Line("3. Editar funcionário");
      Put_Line("4. Exibir funcionários");
      Put_Line("5. Sair");

      Get(Opcao);

      case Opcao is
         when '1' =>
            Put("Informe o ID do funcionário: ");
            Get(Id);

            Put("Informe o nome do funcionário: ");
            Get_Line(Novo_Nome);

            Put("Informe o cargo do funcionário: ");
            Get_Line(Novo_Cargo);

            Put("Informe o departamento do funcionário: ");
            Get_Line(Novo_Departamento);

            Put("Informe o salário do funcionário: ");
            Get(Novo_Salario);

            Adicionar_Funcionario(Lista_Func, Criar_Funcionario(Id, Novo_Nome, Novo_Cargo, Novo_Departamento, Novo_Salario));

         when '2' =>
            Put("Informe o ID do funcionário a ser removido: ");
            Get(Id);

            Remover_Funcionario(Lista_Func, Id);

         when '3' =>
            Put("Informe o ID do funcionário a ser editado: ");
            Get(Id);

            Put("Informe o novo ID do funcionário: ");
            Get(Novo_Id);

            Put("Informe o novo nome do funcionário: ");
            Get_Line(Novo_Nome);

            Put("Informe o novo cargo do funcionário: ");
            Get_Line(Novo_Cargo);

            Put("Informe o novo departamento do funcionário: ");
            Get_Line(Novo_Departamento);

            Put("Informe o novo salário do funcionário: ");
            Get(Novo_Salario);

            Editar_Funcionario(Lista_Func, Id, Criar_Funcionario(Novo_Id, Novo_Nome, Novo_Cargo, Novo_Departamento, Novo_Salario));

         when '4' =>
            Exibir_Funcionarios(Lista_Func);

         when '5' =>
            exit;

         when others =>
            Put_Line("Opção inválida.");
      end case;
   end loop;
end Gerenciamento_Funcionarios;
```

Este código em ADA implementa um programa de gerenciamento de funcionários. Ele define um tipo `Funcionario` que contém informações sobre cada funcionário, como ID, nome, cargo, departamento e salário. O programa usa uma lista dinâmica de funcionários para armazenar os funcionários adicionados. As operações disponíveis incluem adicionar, remover, editar e exibir funcionários.

A função `Criar_Funcionario` é usada para criar um novo funcionário com base nas informações fornecidas. A procedimento `Adicionar_Funcionario` adiciona um novo funcionário à lista. O procedimento `Remover_Funcionario` remove um funcionário da lista com base em seu ID. O procedimento `Editar_Funcionario` permite editar as informações de um funcionário existente. A função `Exibir_Funcionarios` exibe todos os funcionários armazenados na lista.

O código principal usa um loop para exibir um menu de opções ao usuário. O usuário pode selecionar uma opção digitando o número correspondente. O loop continua até que o usuário selecione a opção para sair.

Este código complexo em ADA demonstra como criar um programa de gerenciamento de funcionários funcional em uma empresa hipotética. Ele usa recursos avançados da linguagem ADA para garantir a manipulação correta dos dados e a segurança do programa.