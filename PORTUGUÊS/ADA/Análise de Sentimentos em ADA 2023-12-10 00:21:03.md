Claro! Aqui está um código complexo em ADA que realiza a análise de sentimentos em um texto fornecido pelo usuário. O código utiliza uma abordagem baseada em regras para determinar se o sentimento expresso no texto é positivo, negativo ou neutro.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Analise_de_Sentimentos is

   type Sentimento is (Positivo, Negativo, Neutro);

   function Analisar_Texto(Texto : in Unbounded_String) return Sentimento is
      Palavra : Unbounded_String := "";
   begin
      -- Lista de palavras positivas
      Positivas : constant array(1..5) of String := ("bom", "ótimo", "excelente", "maravilhoso", "incrível");
      -- Lista de palavras negativas
      Negativas : constant array(1..5) of String := ("ruim", "horrível", "terrível", "péssimo", "desastroso");
      
      -- Itera sobre cada caractere do texto
      for I in 1 .. Unbounded_String'Length(Texto) loop
         -- Verifica se o caractere atual é uma letra ou espaço
         if (Character'Val(Texto(I)) in Character'Range('A', 'Z'))
            or (Character'Val(Texto(I)) in Character'Range('a', 'z'))
            or (Character'Val(Texto(I)) = Character'Val(' ')) then
            -- Concatena o caractere na palavra em construção
            Palavra := Palavra & Unbounded_String'From_Character(Character'Val(Texto(I)));
         else
            -- Verifica se a palavra em construção está presente na lista de palavras positivas
            for J in 1 .. Positivas'Length loop
               if Palavra = Unbounded_String(Positivas(J)) then
                  return Positivo;
               end if;
            end loop;
            
            -- Verifica se a palavra em construção está presente na lista de palavras negativas
            for J in 1 .. Negativas'Length loop
               if Palavra = Unbounded_String(Negativas(J)) then
                  return Negativo;
               end if;
            end loop;
            
            -- Reinicia a palavra em construção
            Palavra := "";
         end if;
      end loop;
      
      -- Se nenhuma palavra positiva ou negativa foi encontrada, retorna neutro
      return Neutro;
   end Analisar_Texto;

   Texto : Unbounded_String;
   Sent : Sentimento;
begin
   -- Solicita o texto ao usuário
   Put("Digite um texto: ");
   Get_Line(Texto);
   
   -- Chama a função de análise de sentimentos
   Sent := Analisar_Texto(Texto);
   
   -- Imprime o resultado
   case Sent is
      when Positivo =>
         Put_Line("O texto expressa um sentimento positivo.");
      when Negativo =>
         Put_Line("O texto expressa um sentimento negativo.");
      when Neutro =>
         Put_Line("O texto não expressa um sentimento definido.");
   end case;
end Analise_de_Sentimentos;
```

Este código em ADA realiza a análise de sentimentos em um texto fornecido pelo usuário. Ele primeiro define o tipo enumerado `Sentimento` com os valores `Positivo`, `Negativo` e `Neutro`.

A função `Analisar_Texto` recebe o texto a ser analisado e retorna o sentimento expresso no texto. Ela utiliza listas de palavras positivas e negativas para determinar o sentimento.

O programa principal solicita ao usuário que digite um texto, chama a função `Analisar_Texto` passando o texto digitado e imprime o resultado da análise, indicando se o sentimento expresso no texto é positivo, negativo ou neutro.

Esse é um exemplo de código complexo em ADA que utiliza uma abordagem baseada em regras para realizar a análise de sentimentos em um texto.