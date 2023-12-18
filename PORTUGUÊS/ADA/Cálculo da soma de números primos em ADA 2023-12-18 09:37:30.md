Claro! Aqui está um exemplo de um código complexo em ADA que utiliza várias funcionalidades da linguagem para calcular a soma dos números primos em um determinado intervalo. O código está devidamente comentado para facilitar a compreensão:

```
-- Declaração do pacote com as funções úteis
with Ada.Text_IO;
with Ada.Integer_Text_IO;
package Primos is
   function E_Primo(N : Integer) return Boolean;

   procedure Calcular_Soma_Primos(Inicio, Fim : Integer);
end Primos;

-- Implementação do pacote
package body Primos is
   function E_Primo(N : Integer) return Boolean is
      Primo : Boolean := True;
   begin
      if N < 2 then
         Primo := False;
      else
         for I in 2 .. Integer(Sqrt(N)) loop
            if N mod I = 0 then
               Primo := False;
               exit;
            end if;
         end loop;
      end if;
      return Primo;
   end E_Primo;

   procedure Calcular_Soma_Primos(Inicio, Fim : Integer) is
      Soma : Integer := 0;
   begin
      for Num in Inicio .. Fim loop
         if E_Primo(Num) then
            Soma := Soma + Num;
         end if;
      end loop;
      Ada.Text_IO.Put_Line("A soma dos números primos entre " & Integer'Image(Inicio) & " e " & Integer'Image(Fim) & " é: " & Integer'Image(Soma));
   end Calcular_Soma_Primos;
end Primos;

-- Programa principal
with Primos;
with Ada.Text_IO;
procedure Main is
   Inicio, Fim : Integer;
begin
   -- Leitura dos valores de início e fim do intervalo
   Ada.Text_IO.Put("Digite o valor de início do intervalo: ");
   Ada.Integer_Text_IO.Get(Inicio);
   Ada.Text_IO.Put("Digite o valor de fim do intervalo: ");
   Ada.Integer_Text_IO.Get(Fim);
   Ada.Text_IO.New_Line;

   -- Chama a função para calcular a soma dos números primos
   Primos.Calcular_Soma_Primos(Inicio, Fim);
end Main;
```

Este código define um pacote chamado `Primos` que contém duas funções: `E_Primo` e `Calcular_Soma_Primos`. A função `E_Primo` verifica se um número é primo, enquanto a função `Calcular_Soma_Primos` calcula a soma dos números primos em um determinado intervalo.

O programa principal (`Main`) utiliza o pacote `Primos` para ler os valores de início e fim do intervalo e chama a função `Calcular_Soma_Primos` para realizar o cálculo da soma dos números primos. O resultado é exibido no console.

Esse código exemplifica o uso de pacotes, funções, estruturas de controle, operadores aritméticos e de comparação, além de entrada e saída de dados em Ada.