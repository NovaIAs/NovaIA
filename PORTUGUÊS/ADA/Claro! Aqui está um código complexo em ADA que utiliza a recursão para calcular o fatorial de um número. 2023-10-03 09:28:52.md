Claro! Aqui está um código complexo em ADA que utiliza a recursão para calcular o fatorial de um número:

```
with Ada.Text_IO; use Ada.Text_IO;

procedure Fatorial is

   function Calcular_Fatorial (Numero : Natural) return Natural is
   begin
      if Numero = 0 then
         return 1;
      else
         return Numero * Calcular_Fatorial(Numero - 1);
      end if;
   end Calcular_Fatorial;

   Numero_Entrada : Natural;
   Fatorial_Resultado : Natural;

begin
   Put("Digite um número inteiro não negativo: ");
   Get(Numero_Entrada);
   Fatorial_Resultado := Calcular_Fatorial(Numero_Entrada);
   Put("O fatorial de ");
   Put(Numero_Entrada, Width => 0);
   Put(" é ");
   Put(Fatorial_Resultado, Width => 0);
   New_Line;
end Fatorial;
```

Este código em ADA calcula o fatorial de um número fornecido pelo usuário. Ele utiliza uma função recursiva chamada `Calcular_Fatorial`, que recebe um número natural como parâmetro e retorna o fatorial desse número.

A função `Calcular_Fatorial` verifica se o número fornecido é igual a zero. Se for zero, retorna 1, pois o fatorial de 0 é definido como 1. Caso contrário, retorna o produto do número pelo fatorial do número anterior, calculado chamando a própria função `Calcular_Fatorial` com o parâmetro `Numero - 1`.

Na parte principal do programa, é solicitado ao usuário que insira um número inteiro não negativo. O número fornecido pelo usuário é armazenado na variável `Numero_Entrada`. Em seguida, a função `Calcular_Fatorial` é chamada com `Numero_Entrada` como argumento, e o resultado é armazenado na variável `Fatorial_Resultado`.

Por fim, os resultados são exibidos na tela, informando o número digitado e seu fatorial correspondente.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.