Claro! Aqui está um exemplo de código complexo em ADA que realiza uma análise sintática de expressões aritméticas simples:

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Analise_Sintatica is

   -- Definindo o tipo para representar os tokens
   type Token is (Numero, Operador, Parenteses);

   -- Definindo o tipo para representar os elementos da expressão
   type Elemento is
      record
         case Tipo is
            when Numero      => Valor_Numero : Float;
            when Operador    => Valor_Operador : Character;
            when Parenteses  => Valor_Parenteses : Character;
         end case;
      end record;

   -- Definindo o tipo para representar a pilha de elementos
   type Pilha is array (1..100) of Elemento;
   type Indice_Pilha is range 1..100;

   -- Função para verificar se um caracter é um operador válido
   function E_Operador (C : Character) return Boolean is
   begin
      return C in '+' or '-' or '*' or '/';
   end E_Operador;

   -- Função para verificar se um caracter é um número válido
   function E_Numero (C : Character) return Boolean is
   begin
      return C in '0'..'9' or '.';
   end E_Numero;

   -- Função para realizar a análise sintática da expressão
   procedure Analisar_Expressao (Expressao : String) is
      Pilha_Elementos : Pilha;
      Topo_Pilha : Indice_Pilha := 0;
      Token_Atual : Token;
      Elemento_Atual : Elemento;
      Numero_Atual : Float;
      Operador_Atual : Character;

      procedure Empilhar (E : Elemento) is
      begin
         Topo_Pilha := Topo_Pilha + 1;
         Pilha_Elementos(Topo_Pilha) := E;
      end Empilhar;

      procedure Desempilhar is
      begin
         Topo_Pilha := Topo_Pilha - 1;
      end Desempilhar;

   begin
      for I in Expressao'Range loop
         if E_Numero(Expressao(I)) then
            Numero_Atual := Float'Value(Expressao(I));
            Token_Atual := Numero;
            Elemento_Atual := (Tipo => Numero, Valor_Numero => Numero_Atual);
            Empilhar(Elemento_Atual);
         elsif E_Operador(Expressao(I)) then
            Operador_Atual := Expressao(I);
            Token_Atual := Operador;
            Elemento_Atual := (Tipo => Operador, Valor_Operador => Operador_Atual);
            Empilhar(Elemento_Atual);
         elsif Expressao(I) = '(' or Expressao(I) = ')' then
            Token_Atual := Parenteses;
            Elemento_Atual := (Tipo => Parenteses, Valor_Parenteses => Expressao(I));
            Empilhar(Elemento_Atual);
         end if;
      end loop;

      -- Exibir os elementos da pilha
      Put_Line("Elementos na pilha:");
      for I in 1..Topo_Pilha loop
         if Pilha_Elementos(I).Tipo = Numero then
            Put("Número: ");
            Put(Item => Pilha_Elementos(I).Valor_Numero, Fore => 1, Aft => 2, Exp => 0);
            New_Line;
         elsif Pilha_Elementos(I).Tipo = Operador then
            Put("Operador: ");
            Put(Pilha_Elementos(I).Valor_Operador);
            New_Line;
         elsif Pilha_Elementos(I).Tipo = Parenteses then
            Put("Parenteses: ");
            Put(Pilha_Elementos(I).Valor_Parenteses);
            New_Line;
         end if;
      end loop;
   end Analisar_Expressao;

begin
   Analisar_Expressao("(2+3)*(4-1)");
end Analise_Sintatica;
```

Nesse exemplo, temos um programa em ADA que realiza a análise sintática de uma expressão aritmética simples. O programa utiliza tipos de dados, funções e estruturas de controle para realizar essa análise.

O programa começa com a definição dos tipos `Token` e `Elemento`, que representam os tokens da expressão (números, operadores e parênteses) e os elementos da pilha, respectivamente. Em seguida, temos a definição do tipo `Pilha` e `Indice_Pilha`, que representam a pilha de elementos e o índice da pilha.

A análise sintática é realizada pela função `Analisar_Expressao`, que recebe como parâmetro a expressão a ser analisada. Dentro dessa função, é percorrido cada caractere da expressão e, de acordo com o tipo de caractere, é empilhado o elemento correspondente na pilha. Ao final, é exibido na tela os elementos presentes na pilha.

Espero que esse código atenda às suas expectativas de complexidade e diferenciação! Lembre-se que esse é apenas um exemplo e você pode adaptar e modificar o código conforme suas necessidades.