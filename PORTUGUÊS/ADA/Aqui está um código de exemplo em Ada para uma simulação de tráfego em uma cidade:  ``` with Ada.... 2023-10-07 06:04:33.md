Claro! Aqui está um exemplo de um código complexo em Ada:

```
-- Este é um exemplo de um código em Ada que lida com uma simulação de tráfego em uma cidade.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Simulador_Trafego is

   type Veiculo is record
      Marca      : String (1 .. 20);
      Modelo     : String (1 .. 20);
      Velocidade : Float;
   end record;

   type Lista_Veiculos is array (Positive range <>) of Veiculo;

   procedure Adicionar_Veiculo (Lista : in out Lista_Veiculos; Novo_Veiculo : in Veiculo) is
   begin
      Lista (Lista'Last + 1) := Novo_Veiculo;
   end Adicionar_Veiculo;

   procedure Exibir_Veiculos (Lista : in Lista_Veiculos) is
   begin
      for I in Lista'Range loop
         Put_Line ("Marca: " & Lista (I).Marca);
         Put_Line ("Modelo: " & Lista (I).Modelo);
         Put_Line ("Velocidade: " & Float'Image (Lista (I).Velocidade));
         New_Line;
      end loop;
   end Exibir_Veiculos;

   Veiculos : Lista_Veiculos;

begin

   -- Adicionando alguns veículos à lista
   Adicionar_Veiculo (Veiculos, (Marca => "Fiat", Modelo => "Uno", Velocidade => 60.0));
   Adicionar_Veiculo (Veiculos, (Marca => "Chevrolet", Modelo => "Cruze", Velocidade => 80.5));
   Adicionar_Veiculo (Veiculos, (Marca => "Ford", Modelo => "Mustang", Velocidade => 120.0));

   -- Exibindo os veículos adicionados
   Exibir_Veiculos (Veiculos);

end Simulador_Trafego;
```

Neste código, temos uma simulação de tráfego em uma cidade. Temos a definição de um tipo de dado `Veiculo`, que possui os campos `Marca`, `Modelo` e `Velocidade`. Em seguida, temos o tipo `Lista_Veiculos`, que é uma array dinâmica de veículos.

A função `Adicionar_Veiculo` é responsável por adicionar um novo veículo à lista de veículos. A função `Exibir_Veiculos` percorre a lista de veículos e exibe as informações de cada veículo.

No procedimento principal `Simulador_Trafego`, adicionamos alguns veículos à lista `Veiculos` e, em seguida, exibimos todas as informações dos veículos adicionados.

Esse código em Ada demonstra o uso de tipos de dados, arrays, subprogramas, registros e a manipulação de dados. É um exemplo simples de como se pode criar uma simulação de tráfego em uma cidade usando a linguagem Ada.