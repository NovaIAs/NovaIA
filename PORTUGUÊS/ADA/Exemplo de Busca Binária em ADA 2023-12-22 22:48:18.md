Claro! Aqui está um exemplo de um código em ADA que realiza uma busca binária em um vetor de números inteiros. Vou explicar cada parte do código para facilitar o entendimento:

```ada
with Ada.Text_IO;

procedure Busca_Binaria is

   type Vetor is array (Positive range <>) of Integer;

   function Buscar (Arranjo : in Vetor; Elemento : in Integer) return Boolean is
      L, R, Meio : Integer := Arranjo'First, Arranjo'Last, (Arranjo'First + Arranjo'Last) / 2;
   begin
      while L <= R loop
         Meio := (L + R) / 2;
         if Arranjo(Meio) = Elemento then
            return True;
         elsif Arranjo(Meio) < Elemento then
            L := Meio + 1;
         else
            R := Meio - 1;
         end if;
      end loop;
      return False;
   end Buscar;

   procedure Imprimir_Resultado (Encontrado : Boolean) is
   begin
      if Encontrado then
         Ada.Text_IO.Put_Line("Elemento encontrado no vetor.");
      else
         Ada.Text_IO.Put_Line("Elemento não encontrado no vetor.");
      end if;
   end Imprimir_Resultado;

   Arranjo : Vetor(1 .. 10) := (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
   Elemento : Integer := 9;

begin
   if Buscar(Arranjo, Elemento) then
      Imprimir_Resultado(True);
   else
      Imprimir_Resultado(False);
   end if;

end Busca_Binaria;
```

Este código em ADA implementa a busca binária em um vetor de números inteiros para encontrar um elemento específico. Vou explicar cada parte do código:

- Na linha 3, declaramos o tipo Vetor como um array de inteiros.

- Na linha 5, definimos a função Buscar, que recebe um arranjo (vetor) e um elemento a ser procurado, e retorna um valor booleano indicando se o elemento foi encontrado ou não. 

- Na linha 6, declaramos as variáveis L (índice esquerdo), R (índice direito) e Meio (índice do elemento do meio do arranjo). Inicializamos L com o primeiro índice do arranjo, R com o último índice do arranjo e Meio com a média entre L e R.

- Dentro do loop while (linhas 8 a 18), verificamos se o valor do arranjo no índice Meio é igual ao elemento procurado. Se for, retornamos True. Caso contrário, verificamos se o valor do arranjo no índice Meio é menor que o elemento procurado. Se for, atualizamos L para Meio + 1. Se não for, atualizamos R para Meio - 1.

- Finalmente, se chegarmos ao final do loop sem encontrar o elemento, retornamos False.

- Na linha 20, temos o procedimento Imprimir_Resultado, que recebe um valor booleano (indicando se o elemento foi encontrado ou não) e imprime uma mensagem correspondente.

- Na linha 23, declaramos o arranjo Vetor com 10 elementos e inicializamos com valores.

- Na linha 24, declaramos a variável Elemento com o valor a ser buscado no arranjo.

- A partir da linha 26, fazemos a chamada para a função Buscar passando o Arranjo e o Elemento. Dependendo do valor retornado, chamamos o procedimento Imprimir_Resultado com o valor True ou False.

Este código em ADA é uma implementação simples da busca binária e pode ser adaptado para lidar com arranjos de diferentes tamanhos e tipos de elementos.