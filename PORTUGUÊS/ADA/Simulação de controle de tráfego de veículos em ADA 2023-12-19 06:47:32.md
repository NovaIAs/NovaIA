Claro! Aqui está um exemplo de um código complexo em ADA, que realiza uma simulação de um sistema de controle de tráfego de veículos:

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Controle_Trafego is

   type Veiculo is record
      placa  : String := "";
      velocidade : Integer := 0;
   end record;

   type Lista_Veiculos is new Vector(Veiculo_Index, Veiculo);

   procedure Adicionar_Veiculo(Lista : in out Lista_Veiculos; V : in Veiculo) is
   begin
      Lista.Append(V);
   end Adicionar_Veiculo;

   procedure Remover_Veiculo(Lista : in out Lista_Veiculos; Placa : in String) is
      Pos : Veiculo_Index;
   begin
      for I in Lista'Range loop
         if Lista(I).placa = Placa then
            Lista.Delete(I);
            exit;
         end if;
      end loop;
   end Remover_Veiculo;

   procedure Imprimir_Lista(Lista : in Lista_Veiculos) is
   begin
      for I in Lista'Range loop
         Put_Line("Placa: " & Lista(I).placa & ", Velocidade: " & Lista(I).velocidade'Image);
      end loop;
   end Imprimir_Lista;

   procedure Simulacao is
      Lista : Lista_Veiculos;
      Opcao : Integer;
      Placa : String(1..10);
      Velocidade : Integer;
   begin
      loop
         Put_Line("1 - Adicionar veículo");
         Put_Line("2 - Remover veículo");
         Put_Line("3 - Imprimir lista de veículos");
         Put_Line("0 - Sair");
         Put("Opção: ");
         Get(Opcao);

         case Opcao is
            when 1 =>
               Put("Placa: ");
               Get_Line(Placa);
               Put("Velocidade: ");
               Get(Velocidade);
               Adicionar_Veiculo(Lista, (placa => Placa, velocidade => Velocidade));
            when 2 =>
               Put("Placa: ");
               Get_Line(Placa);
               Remover_Veiculo(Lista, Placa);
            when 3 =>
               Imprimir_Lista(Lista);
            when 0 =>
               exit;
            when others =>
               Put_Line("Opção inválida!");
         end case;
      end loop;
   end Simulacao;

begin
   Simulacao;
end Controle_Trafego;
```

Neste código, utilizamos a linguagem ADA para implementar um sistema de controle de tráfego de veículos. A estrutura principal é o tipo `Veiculo`, que possui campos para armazenar a placa e a velocidade de um veículo.

A lista de veículos é implementada através do tipo `Lista_Veiculos`, definido como um vetor de veículos. As funções `Adicionar_Veiculo` e `Remover_Veiculo` são responsáveis por adicionar e remover veículos da lista, respectivamente.

A função `Imprimir_Lista` percorre a lista de veículos e imprime as informações de cada veículo na saída.

A função `Simulacao` é a parte principal do código, onde é exibido um menu com opções para adicionar veículos, remover veículos, imprimir a lista de veículos ou sair do programa. Dependendo da opção selecionada, as funções correspondentes são chamadas.

Por fim, a função `Controle_Trafego` é responsável por chamar a função `Simulacao` para iniciar a execução do programa.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição.