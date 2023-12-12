Claro! Aqui está um exemplo de um código complexo em ADA, feito especialmente para você:

```
-- Este é um exemplo de um código complexo em ADA que utiliza várias funcionalidades da linguagem para resolver um problema específico.

with Ada.Text_IO; use Ada.Text_IO;

procedure Complexo is

   -- Declaração de tipos de dados personalizados
   type DiaSemana is (Domingo, Segunda, Terca, Quarta, Quinta, Sexta, Sabado);
   type Vetor is array (Positive range <>) of Integer;

   -- Função recursiva para calcular o fatorial de um número
   function Fatorial(N: Natural) return Natural is
   begin
      if N = 0 then
         return 1;
      else
         return N * Fatorial(N - 1);
      end if;
   end Fatorial;

   -- Procedimento para imprimir o vetor na tela
   procedure Imprimir(V: in Vetor) is
   begin
      Put("Vetor: ");
      for I in V'Range loop
         Put(V(I)'Image & " ");
      end loop;
      New_Line;
   end Imprimir;

begin
   -- Declaração de variáveis locais
   Dia: DiaSemana := Domingo;
   Numero: Integer := 42;
   FatorialNumero: Natural;
   MeuVetor: Vetor(1..5) := (10, 20, 30, 40, 50);

   -- Utilização de estruturas de controle condicionais
   if Dia = Sabado or Dia = Domingo then
      Put_Line("Hoje é fim de semana!");
   else
      Put_Line("Hoje é dia de trabalho.");
   end if;

   -- Utilização de estruturas de controle de repetição
   for I in 1..Numero loop
      Put(I'Image & " ");
   end loop;
   New_Line;

   -- Chamada da função recursiva Fatorial
   FatorialNumero := Fatorial(Numero);
   Put_Line("O fatorial de " & Numero'Image & " é " & FatorialNumero'Image);

   -- Chamada do procedimento Imprimir
   Imprimir(MeuVetor);

   -- Utilização de exceções
   begin
      Numero := Numero / 0;
   exception
      when Constraint_Error =>
         Put_Line("Erro: divisão por zero!");
   end;

   -- Utilização de tipos de dados compostos
   declare
      type Registro is record
         Nome: String(1..10);
         Idade: Natural;
      end record;

      Pessoas: array (1..3) of Registro := (("Joao", 20), ("Maria", 25), ("Pedro", 30));
   begin
      for Pessoa in Pessoas'Range loop
         Put_Line("Nome: " & Pessoas(Pessoa).Nome & ", Idade: " & Pessoas(Pessoa).Idade'Image);
      end loop;
   end;

end Complexo;
```

Este código é um exemplo complexo em ADA que utiliza várias funcionalidades da linguagem. Ele inclui a declaração de tipos de dados personalizados, uma função recursiva para calcular o fatorial de um número, um procedimento para imprimir um vetor, estruturas de controle condicionais e de repetição, exceções, tipos de dados compostos (registro) e mais.

No código, é declarado um tipo de dado chamado `DiaSemana` que representa os dias da semana. Também é declarado um tipo de dado `Vetor` que representa um vetor de inteiros.

O código inclui uma função recursiva chamada `Fatorial` que calcula o fatorial de um número. Ela utiliza uma estrutura `if` para tratar o caso base da recursão.

Há também um procedimento chamado `Imprimir` que recebe um vetor como parâmetro e o imprime na tela usando um loop `for`.

O código principal declara algumas variáveis locais, como `Dia` que recebe o valor `Domingo`, `Numero` que recebe o valor 42 e `MeuVetor` que é inicializado com os valores (10, 20, 30, 40, 50).

Há uma estrutura `if` que verifica se o `Dia` é um fim de semana e imprime uma mensagem correspondente.

Em seguida, há um loop `for` que imprime os números de 1 a `Numero`.

A função `Fatorial` é chamada passando o valor de `Numero` como argumento e o resultado é armazenado em `FatorialNumero`. A função `Put_Line` é usada para imprimir o resultado na tela.

O procedimento `Imprimir` é chamado passando `MeuVetor` como argumento.

O código também inclui um bloco `begin` e `end` que trata uma exceção `Constraint_Error` caso ocorra uma divisão por zero. Uma mensagem de erro é impressa na tela.

Por fim, há outro bloco `begin` e `end` que utiliza um tipo de dado composto chamado `Registro` para armazenar informações de pessoas. Um vetor de registros é declarado e percorrido com um loop `for`. As informações são impressas na tela usando `Put_Line`.

Espero que este exemplo atenda às suas expectativas de um código complexo em ADA!