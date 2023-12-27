Claro! Aqui está um exemplo de código em ADA que implementa um algoritmo para calcular o fatorial de um número:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Fatorial is

   function Calcula_Fatorial(N : Integer) return Integer is
      Resultado : Integer := 1;
   begin
      -- Verifica se o número é zero ou negativo
      if N <= 0 then
         raise Program_Error with "Número inválido!";
      end if;

      -- Calcula o fatorial
      for I in 1 .. N loop
         Resultado := Resultado * I;
      end loop;

      return Resultado;
   end Calcula_Fatorial;

   Numero : Integer;
begin
   -- Solicita o número para calcular o fatorial
   Put("Digite um número inteiro positivo: ");
   Get(Numero);

   -- Chama a função para calcular o fatorial e exibe o resultado
   Put("O fatorial de ");
   Put(Numero);
   Put(" é ");
   Put(Calcula_Fatorial(Numero));
   New_Line;
exception
   when Program_Error =>
      Put_Line("Erro: Número inválido!");
end Fatorial;
```

Este código em ADA implementa uma função chamada `Calcula_Fatorial` que recebe um número inteiro como parâmetro e retorna o fatorial desse número. O programa principal solicita ao usuário um número inteiro positivo, chama a função `Calcula_Fatorial` passando esse número como argumento e exibe o resultado na tela.

O código também inclui tratamento de exceção para números inválidos (zero ou negativos). Caso o usuário digite um número inválido, o programa exibirá uma mensagem de erro.

Espero que este exemplo atenda às suas expectativas! Se você precisar de mais alguma coisa, é só me pedir.