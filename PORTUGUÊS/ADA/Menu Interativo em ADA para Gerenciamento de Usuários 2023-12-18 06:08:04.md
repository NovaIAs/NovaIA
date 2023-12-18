Claro! Aqui está um código complexo em ADA, escrito em português, que visa demonstrar a utilização de vários recursos da linguagem.

```ada
-- Este programa em ADA exibe um menu interativo para o usuário, permitindo que ele escolha entre diferentes opções.
-- Cada opção realiza uma tarefa específica, demonstrando o uso de estruturas de controle, tipos e manipulação de arquivos.

with Ada.Text_IO;  -- Biblioteca para entrada/saída de texto
with Ada.Strings;  -- Biblioteca para manipulação de strings

procedure Menu is
   -- Tipo enum para representar as opções do menu
   type Opcao is (Opcao1, Opcao2, Opcao3, Opcao4);
   
   -- Tipo registro para armazenar os dados dos usuários
   type Usuario is record
      Nome  : String(1 .. 50);
      Idade : Integer;
   end record;
   
   -- Tipo array para armazenar os usuários
   type Lista_Usuarios is array (Positive range <>) of Usuario;
   
   -- Variáveis globais
   Opcao_Selecionada : Opcao;
   Usuarios          : Lista_Usuarios(1 .. 10);  -- Array com limite de 10 usuários
   Indice_Usuario    : Natural := 1;             -- Índice atual do usuário (inicia em 1)
   
   -- Função para obter uma opção válida do usuário
   function Ler_Opcao return Opcao is
      Opcao_Lida : Opcao;
   begin
      loop
         Ada.Text_IO.Put_Line("Selecione uma opção:");
         Ada.Text_IO.Put_Line("1 - Cadastrar usuário");
         Ada.Text_IO.Put_Line("2 - Exibir lista de usuários");
         Ada.Text_IO.Put_Line("3 - Alterar idade de um usuário");
         Ada.Text_IO.Put_Line("4 - Sair");
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("Opção: ");
         
         -- Lê a opção digitada pelo usuário
         case Ada.Text_IO.Integer_IO.Get'Input is  -- Usa Integer_IO.Get para ler um Integer
            when 1 => Opcao_Lida := Opcao1;
            when 2 => Opcao_Lida := Opcao2;
            when 3 => Opcao_Lida := Opcao3;
            when 4 => Opcao_Lida := Opcao4;
            when others => null;  -- Ignora opções inválidas
         end case;
         
         exit when Opcao_Lida in Opcao1 .. Opcao4;  -- Sai do loop se opção válida
         
         Ada.Text_IO.Put_Line("Opção inválida! Por favor, selecione uma opção válida.");
         Ada.Text_IO.New_Line;
      end loop;
      
      return Opcao_Lida;
   end Ler_Opcao;
   
   -- Procedimento para cadastrar um novo usuário
   procedure Cadastrar_Usuario is
      Nome_Usuario  : String(1 .. 50);
      Idade_Usuario : Integer;
   begin
      Ada.Text_IO.Put("Digite o nome do usuário: ");
      Ada.Text_IO.Get_Line(Nome_Usuario);
      
      Ada.Text_IO.Put("Digite a idade do usuário: ");
      Ada.Text_IO.Integer_IO.Get(Idade_Usuario);
      
      -- Armazena os dados do usuário na lista
      Usuarios(Indice_Usuario).Nome  := Nome_Usuario;
      Usuarios(Indice_Usuario).Idade := Idade_Usuario;
      
      Ada.Text_IO.Put_Line("Usuário cadastrado com sucesso!");
      Ada.Text_IO.New_Line;
      
      Indice_Usuario := Indice_Usuario + 1;  -- Incrementa o índice para o próximo usuário
   end Cadastrar_Usuario;
   
   -- Procedimento para exibir a lista de usuários cadastrados
   procedure Exibir_Lista_Usuarios is
   begin
      if Indice_Usuario = 1 then
         Ada.Text_IO.Put_Line("Não há usuários cadastrados.");
      else
         Ada.Text_IO.Put_Line("Lista de usuários cadastrados:");
         Ada.Text_IO.New_Line;
         
         for I in 1 .. Indice_Usuario - 1 loop
            Ada.Text_IO.Put_Line("Usuário " & I'Image & ":");
            Ada.Text_IO.Put("Nome: ");
            Ada.Text_IO.Put_Line(Usuarios(I).Nome);
            Ada.Text_IO.Put("Idade: ");
            Ada.Text_IO.Integer_IO.Put(Usuarios(I).Idade);
            Ada.Text_IO.New_Line;
         end loop;
      end if;
      
      Ada.Text_IO.New_Line;
   end Exibir_Lista_Usuarios;
   
   -- Procedimento para alterar a idade de um usuário
   procedure Alterar_Idade_Usuario is
      Indice_Usuario_Alterar : Positive;
      Nova_Idade             : Integer;
   begin
      if Indice_Usuario = 1 then
         Ada.Text_IO.Put_Line("Não há usuários cadastrados.");
      else
         Ada.Text_IO.Put("Digite o índice do usuário que deseja alterar a idade: ");
         Ada.Text_IO.Integer_IO.Get(Indice_Usuario_Alterar);
         
         -- Verifica se o índice é válido
         if Indice_Usuario_Alterar > 0 and then Indice_Usuario_Alterar < Indice_Usuario then
            Ada.Text_IO.Put("Digite a nova idade do usuário: ");
            Ada.Text_IO.Integer_IO.Get(Nova_Idade);
            
            -- Altera a idade do usuário
            Usuarios(Indice_Usuario_Alterar).Idade := Nova_Idade;
            
            Ada.Text_IO.Put_Line("Idade do usuário alterada com sucesso!");
         else
            Ada.Text_IO.Put_Line("Índice inválido! Por favor, digite um índice válido.");
         end if;
      end if;
      
      Ada.Text_IO.New_Line;
   end Alterar_Idade_Usuario;
   
begin
   -- Loop principal do programa
   loop
      -- Exibe o menu e lê a opção selecionada pelo usuário
      Opcao_Selecionada := Ler_Opcao;
      
      -- Executa a ação correspondente à opção selecionada
      case Opcao_Selecionada is
         when Opcao1 => Cadastrar_Usuario;
         when Opcao2 => Exibir_Lista_Usuarios;
         when Opcao3 => Alterar_Idade_Usuario;
         when Opcao4 => exit;  -- Sai do loop principal para encerrar o programa
      end case;
   end loop;
end Menu;
```

Este código em ADA apresenta um menu interativo para o usuário, no qual ele pode selecionar diferentes opções. Cada opção realiza uma tarefa específica:

1. Cadastrar usuário: o usuário pode digitar o nome e a idade de um novo usuário, que será armazenado em um array de usuários.
2. Exibir lista de usuários: o programa exibe na tela a lista de usuários cadastrados até o momento, mostrando nome e idade.
3. Alterar idade de um usuário: o usuário pode selecionar um usuário pelo índice e alterar sua idade.
4. Sair: encerra o programa.

O código utiliza recursos da linguagem ADA, como tipos enum, registros, arrays e manipulação de strings. Além disso, ele faz uso de estruturas de controle como loops e condicionais (case) para implementar a lógica do menu.