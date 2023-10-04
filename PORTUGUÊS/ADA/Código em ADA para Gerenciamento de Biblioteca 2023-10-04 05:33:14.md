Claro! Aqui está um código em ADA que simula um sistema de gerenciamento de biblioteca. Ele permite que os usuários cadastrem livros, consultem o acervo, façam empréstimos e devoluções. Vou explicar cada parte do código em detalhes:

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Biblioteca is

   -- Definição do tipo Livro
   type Livro is record
      Titulo : String(1 .. 100);
      Autor  : String(1 .. 100);
      Ano    : Integer;
      Disponivel : Boolean := True;
   end record;

   -- Definição do tipo Acervo
   type Acervo is array (Integer range <>) of Livro;

   -- Declaração do acervo da biblioteca
   Acervo_Biblioteca : Acervo (1 .. 100);

   -- Variável para controlar a quantidade de livros
   Quantidade_Livros : Integer := 0;

   -- Procedimento para cadastrar um novo livro
   procedure Cadastrar_Livro is
      Livro_Novo : Livro;
   begin
      Put_Line("---- Cadastro de Livro ----");
      Put("Título: ");
      Get_Line(Livro_Novo.Titulo);
      Put("Autor: ");
      Get_Line(Livro_Novo.Autor);
      Put("Ano de Publicação: ");
      Get(Livro_Novo.Ano);
      Livro_Novo.Disponivel := True;
      
      Quantidade_Livros := Quantidade_Livros + 1;
      Acervo_Biblioteca(Quantidade_Livros) := Livro_Novo;
      
      Put_Line("Livro cadastrado com sucesso!");
   end Cadastrar_Livro;

   -- Procedimento para consultar o acervo da biblioteca
   procedure Consultar_Acervo is
   begin
      Put_Line("---- Acervo da Biblioteca ----");
      if Quantidade_Livros = 0 then
         Put_Line("A biblioteca não possui livros cadastrados.");
      else
         for i in 1 .. Quantidade_Livros loop
            Put_Line("Livro " & Integer'Image(i) & ":");
            Put_Line("  Título: " & Acervo_Biblioteca(i).Titulo);
            Put_Line("  Autor: " & Acervo_Biblioteca(i).Autor);
            Put_Line("  Ano de Publicação: " & Integer'Image(Acervo_Biblioteca(i).Ano));
            if Acervo_Biblioteca(i).Disponivel then
               Put_Line("  Disponível: Sim");
            else
               Put_Line("  Disponível: Não");
            end if;
            New_Line;
         end loop;
      end if;
   end Consultar_Acervo;

   -- Procedimento para realizar um empréstimo de livro
   procedure Emprestar_Livro is
      Livro_Emprestado : Livro;
      Indice_Livro : Integer;
   begin
      Put("Informe o número do livro que deseja emprestar: ");
      Get(Indice_Livro);
      
      if Indice_Livro <= Quantidade_Livros and then Indice_Livro >= 1 then
         Livro_Emprestado := Acervo_Biblioteca(Indice_Livro);
         if Livro_Emprestado.Disponivel then
            Livro_Emprestado.Disponivel := False;
            Acervo_Biblioteca(Indice_Livro) := Livro_Emprestado;
            Put_Line("Livro emprestado com sucesso!");
         else
            Put_Line("Este livro já está emprestado.");
         end if;
      else
         Put_Line("Livro inválido.");
      end if;
   end Emprestar_Livro;

   -- Procedimento para realizar a devolução de um livro
   procedure Devolver_Livro is
      Livro_Devolvido : Livro;
      Indice_Livro : Integer;
   begin
      Put("Informe o número do livro que deseja devolver: ");
      Get(Indice_Livro);
      
      if Indice_Livro <= Quantidade_Livros and then Indice_Livro >= 1 then
         Livro_Devolvido := Acervo_Biblioteca(Indice_Livro);
         if not Livro_Devolvido.Disponivel then
            Livro_Devolvido.Disponivel := True;
            Acervo_Biblioteca(Indice_Livro) := Livro_Devolvido;
            Put_Line("Livro devolvido com sucesso!");
         else
            Put_Line("Este livro já está disponível.");
         end if;
      else
         Put_Line("Livro inválido.");
      end if;
   end Devolver_Livro;

   -- Procedimento principal do programa
   procedure Principal is
      Opcao : Integer;
   begin
      loop
         Put_Line("---- Biblioteca ----");
         Put_Line("Escolha uma opção:");
         Put_Line("1 - Cadastrar Livro");
         Put_Line("2 - Consultar Acervo");
         Put_Line("3 - Emprestar Livro");
         Put_Line("4 - Devolver Livro");
         Put_Line("0 - Sair");
         Put("Opção: ");
         Get(Opcao);

         case Opcao is
            when 1 =>
               Cadastrar_Livro;
            when 2 =>
               Consultar_Acervo;
            when 3 =>
               Emprestar_Livro;
            when 4 =>
               Devolver_Livro;
            when 0 =>
               exit;
            when others =>
               Put_Line("Opção inválida.");
         end case;

         New_Line;
      end loop;
   end Principal;

begin
   Principal;
end Biblioteca;
```

Explicação do código:

1. Primeiro, definimos o tipo `Livro` com os atributos `Titulo`, `Autor`, `Ano` e `Disponivel`. O atributo `Disponivel` indica se o livro está disponível para empréstimo ou não.

2. Em seguida, definimos o tipo `Acervo`, que é um array de `Livro`. Ele representa o acervo da biblioteca.

3. Declaramos a variável `Acervo_Biblioteca` como um `Acervo` com tamanho máximo de 100 livros.

4. Criamos a variável `Quantidade_Livros` para controlar a quantidade de livros cadastrados na biblioteca.

5. Em seguida, temos o procedimento `Cadastrar_Livro`, que permite ao usuário cadastrar um novo livro. Ele solicita as informações do título, autor e ano de publicação, e atribui o valor `True` ao atributo `Disponivel`. A variável `Quantidade_Livros` é incrementada e o livro é armazenado no `Acervo_Biblioteca`.

6. O procedimento `Consultar_Acervo` exibe todos os livros cadastrados na biblioteca. Se não houver livros cadastrados, uma mensagem de aviso é exibida. Caso contrário, o loop for percorre o `Acervo_Biblioteca` e exibe as informações de cada livro.

7. O procedimento `Emprestar_Livro` permite que o usuário empreste um livro. O número do livro desejado é solicitado e verificado se é válido. Se o livro estiver disponível, ele é emprestado e o atributo `Disponivel` é alterado para `False`. Caso contrário, uma mensagem de aviso é exibida.

8. O procedimento `Devolver_Livro` permite que o usuário devolva um livro. O número do livro desejado é solicitado e verificado se é válido. Se o livro estiver emprestado, ele é devolvido e o atributo `Disponivel` é alterado para `True`. Caso contrário, uma mensagem de aviso é exibida.

9. O procedimento `Principal` é o ponto de entrada do programa. Ele exibe um menu com as opções disponíveis e permite que o usuário escolha uma opção. Dependendo da opção escolhida, o procedimento correspondente é chamado. O loop continua até que o usuário escolha a opção "0" para sair.

10. Por fim, no procedimento `Biblioteca`, chamamos o procedimento `Principal` para iniciar o programa.

Esse código em ADA simula um sistema de gerenciamento de biblioteca, permitindo o cadastro de livros, consulta do acervo, empréstimo e devolução de livros. É um exemplo de código complexo em ADA, com funcionalidades completas e uma estrutura organizada.