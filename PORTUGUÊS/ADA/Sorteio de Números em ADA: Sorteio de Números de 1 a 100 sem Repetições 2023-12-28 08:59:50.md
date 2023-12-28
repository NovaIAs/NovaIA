Claro! Aqui está um exemplo de código em ADA que realiza um sorteio de números de 1 a 100 sem repetições. O código utiliza um algoritmo de Fisher-Yates para embaralhar os números antes de selecionar um número aleatório de cada vez. Vou explicar o código passo a passo:

```ada
with Ada.Text_IO;

procedure Sorteio is

   type Vetor is array (1 .. 100) of Integer;
   type VetorEmbaralhado is array (1 .. 100) of Integer;

   procedure Embaralhar (Original : in out Vetor; Embaralhado : out VetorEmbaralhado) is
      Posicao : Integer;
   begin
      for i in Original'Range loop
         Embaralhado(i) := Original(i);
      end loop;

      for i in Embaralhado'Last downto 2 loop
         Posicao := Integer'Random(i);
         Embaralhado(i) := Embaralhado(Posicao);
         Embaralhado(Posicao) := Embaralhado(i);
      end loop;
   end Embaralhar;

   procedure Imprimir (V : VetorEmbaralhado) is
   begin
      for i in V'Range loop
         Ada.Text_IO.Put(Integer'Image(V(i)) & " ");
      end loop;
      Ada.Text_IO.New_Line;
   end Imprimir;

   procedure RealizarSorteio is
      Original : Vetor := (1 .. 100 => 0);
      Embaralhado : VetorEmbaralhado;
      Numero : Integer;
      Contador : Integer := 0;
   begin
      -- Preenche o vetor original com os números de 1 a 100
      for i in Original'Range loop
         Original(i) := i;
      end loop;

      -- Embaralha os números
      Embaralhar(Original, Embaralhado);

      -- Realiza o sorteio
      loop
         Contador := Contador + 1;
         Numero := Embaralhado(Contador);
         Ada.Text_IO.Put("Sorteio " & Integer'Image(Contador) & ": ");
         Ada.Text_IO.Put(Integer'Image(Numero));
         Ada.Text_IO.New_Line;

         exit when Contador = 100;
      end loop;
   end RealizarSorteio;

begin
   RealizarSorteio;
end Sorteio;
```

Explicação do código:

1. Começamos definindo o pacote `Ada.Text_IO` para poder imprimir na tela.

2. Em seguida, definimos o procedimento principal chamado `Sorteio`.

3. Dentro do procedimento `Sorteio`, declaramos dois tipos de dados: `Vetor` e `VetorEmbaralhado`. `Vetor` é um array de inteiros de 1 a 100, enquanto `VetorEmbaralhado` é um array de inteiros de 1 a 100 utilizado para armazenar os números embaralhados.

4. Em seguida, definimos o procedimento `Embaralhar`, que recebe um vetor original e um vetor vazio, e preenche o vetor vazio com os números do vetor original, mas embaralhados. O algoritmo de Fisher-Yates é utilizado para realizar o embaralhamento.

5. O procedimento `Imprimir` é definido para imprimir os números do vetor embaralhado na tela.

6. O procedimento `RealizarSorteio` é o responsável por executar todo o sorteio. Ele começa criando o vetor original com os números de 1 a 100.

7. Em seguida, chama o procedimento `Embaralhar` para embaralhar os números.

8. O sorteio é realizado em um loop, onde a cada iteração é selecionado um número do vetor embaralhado e impresso na tela.

9. O loop é encerrado quando todos os 100 números forem sorteados.

10. Por fim, no procedimento principal `Sorteio`, chamamos o procedimento `RealizarSorteio` para iniciar o sorteio.

Esse código em ADA é um exemplo complexo e diferenciado que dificilmente se repetirá em outros contextos. Ele utiliza recursos da linguagem para realizar o sorteio de números sem repetições, demonstrando o uso de arrays, loops, condicionais e procedimentos em ADA.