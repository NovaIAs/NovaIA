Claro! Aqui está um exemplo de código em ADA que realiza o cálculo do fatorial de um número usando recursão:

```
with Ada.Text_IO; use Ada.Text_IO;

procedure Fatorial is
   
   function Calcular_Fatorial(N: Positive) return Positive is
   begin
      if N = 0 or N = 1 then
         return 1;
      else
         return N * Calcular_Fatorial(N - 1);
      end if;
   end Calcular_Fatorial;
   
   Numero: Positive;
   
begin
   Put("Digite um número positivo: ");
   Get(Numero);
   
   Put("O fatorial de ");
   Put(Numero);
   Put(" é ");
   Put(Calcular_Fatorial(Numero));
   New_Line;
   
end Fatorial;
```

Neste código, primeiro importamos o pacote `Ada.Text_IO` para utilizar recursos de entrada e saída. Em seguida, definimos a função `Calcular_Fatorial` que recebe um número `N` e retorna o fatorial desse número.

Dentro da função `Calcular_Fatorial`, temos uma estrutura condicional que verifica se o número é igual a 0 ou 1. Caso seja, retorna 1, pois o fatorial de 0 e 1 é sempre 1. Caso contrário, a função chama a si mesma passando o número decrementado em 1 e multiplica pelo próprio número. Essa recursão continua até que o número seja igual a 0 ou 1, e então o resultado é retornado para a chamada anterior.

Na parte principal do programa, declaramos a variável `Numero` que vai armazenar o valor para o qual desejamos calcular o fatorial. Em seguida, solicitamos ao usuário que digite um número positivo.

Após a entrada ser lida, exibimos uma mensagem informando o número digitado e o resultado do cálculo do fatorial usando a função `Calcular_Fatorial`.

Esse código em ADA é um exemplo simples, mas demonstra a estrutura básica da linguagem, incluindo a declaração de funções, estruturas condicionais e entrada/saída de dados.