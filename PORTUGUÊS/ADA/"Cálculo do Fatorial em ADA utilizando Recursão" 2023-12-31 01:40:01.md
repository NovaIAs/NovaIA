Claro! Aqui está um código complexo em ADA, que utiliza recursão para calcular o fatorial de um número:

```ada
with Ada.Text_IO;

procedure Fatorial is

   function Calcular_Fatorial(N : Positive) return Natural is
   begin
      if N = 0 then
         return 1;
      else
         return N * Calcular_Fatorial(N - 1);
      end if;
   end Calcular_Fatorial;

   N : Positive;
begin
   Ada.Text_IO.Put("Digite um número positivo: ");
   Ada.Text_IO.Get(N);

   Ada.Text_IO.Put("O fatorial de ");
   Ada.Text_IO.Put(N);
   Ada.Text_IO.Put(" é ");
   Ada.Text_IO.Put(Calcular_Fatorial(N));
   Ada.Text_IO.New_Line;
end Fatorial;
```

Neste código em ADA, temos o procedimento principal `Fatorial`, que utiliza a função `Calcular_Fatorial` para calcular o fatorial de um número fornecido pelo usuário.

A função `Calcular_Fatorial` é implementada de forma recursiva. Ela recebe um número `N` como parâmetro e verifica se `N` é igual a 0. Se for, retorna 1 (caso base). Caso contrário, realiza a chamada recursiva passando `N - 1`. A cada chamada recursiva, o valor de `N` é decrementado até atingir o caso base.

No procedimento principal, é solicitado ao usuário que digite um número positivo. Esse número é lido e armazenado na variável `N`. Em seguida, o programa imprime a mensagem "O fatorial de ", seguida do número digitado, e a mensagem " é ", seguida do resultado do cálculo do fatorial utilizando a função `Calcular_Fatorial`. Por fim, uma nova linha é adicionada para melhorar a exibição do resultado.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.