Certamente! Aqui está um código complexo em ADA que utiliza a biblioteca GNAT.Sockets para criar um servidor TCP/IP. Vou explicar cada parte do código para facilitar o entendimento:

```ada
with GNAT.Sockets;
with Ada.Text_IO;

procedure Servidor_TCP is
   package Socket_IO is new GNAT.Sockets.I_O (Socket_Type => GNAT.Sockets.Stream_Socket);

   subtype Port_Num is Positive range 1..65535;
   Porta : Port_Num := 1234;

   procedure Tratar_Conexao (Socket : GNAT.Sockets.Socket_Type) is
      Mensagem : constant String := "Bem-vindo ao Servidor TCP!";

      S : Socket_IO.Socket_Desc;
      Buf : String (1..256);
      Tam : Socket_IO.Count;
   begin
      Socket_IO.Set_Socket (S, Socket);

      -- Envia a mensagem de boas-vindas ao cliente
      Socket_IO.Write (S, Mensagem & Ada.Text_IO.New_Line);

      -- Espera pela resposta do cliente
      Tam := Socket_IO.Read (S, Buf, Buf'Length);

      -- Exibe a mensagem recebida do cliente
      Ada.Text_IO.Put_Line ("Mensagem recebida do cliente: " & Buf (1..Tam));

      -- Fecha a conexão com o cliente
      Socket_IO.Close_Socket (S);
   exception
      when GNAT.Sockets.Error =>
         Ada.Text_IO.Put_Line ("Erro ao tratar a conexão com o cliente");
   end Tratar_Conexao;

   procedure Aguardar_Conexoes (Socket : GNAT.Sockets.Socket_Type) is
      Cliente : GNAT.Sockets.Socket_Type;
   begin
      loop
         -- Aguarda uma conexão de cliente
         Cliente := GNAT.Sockets.Accept (Socket);

         -- Processa a conexão em uma nova tarefa
         GNAT.Sockets.Spawn (Tratar_Conexao, Cliente);
      end loop;
   end Aguardar_Conexoes;

   function Criar_Socket_TCP return GNAT.Sockets.Socket_Type is
      Socket : GNAT.Sockets.Socket_Type;
   begin
      -- Cria um socket TCP/IP
      Socket := GNAT.Sockets.Socket (Family => GNAT.Sockets.AF_INET, Protocol => GNAT.Sockets.IPPROTO_TCP);

      -- Associa o socket à porta especificada
      GNAT.Sockets.Bind_Socket (Socket, Port => Porta);

      -- Coloca o socket no modo de escuta
      GNAT.Sockets.Listen (Socket);

      return Socket;
   exception
      when GNAT.Sockets.Error =>
         Ada.Text_IO.Put_Line ("Erro ao criar o socket TCP/IP");
         return GNAT.Sockets.Invalid_Socket;
   end Criar_Socket_TCP;

   Servidor : GNAT.Sockets.Socket_Type;
begin
   -- Cria o socket TCP/IP
   Servidor := Criar_Socket_TCP;

   if Servidor /= GNAT.Sockets.Invalid_Socket then
      Ada.Text_IO.Put_Line ("Servidor TCP/IP iniciado na porta " & Porta'Image);

      -- Aguarda conexões de clientes
      Aguardar_Conexoes (Servidor);

      -- Fecha o socket do servidor
      GNAT.Sockets.Close_Socket (Servidor);
   end if;
end Servidor_TCP;
```

Explicação do código:
1. A linha `with GNAT.Sockets;` importa a biblioteca `GNAT.Sockets`, que contém as funcionalidades para criar sockets TCP/IP.
2. A linha `with Ada.Text_IO;` importa a biblioteca `Ada.Text_IO`, que será usada para exibir mensagens na saída do console.
3. O procedimento `Tratar_Conexao` é responsável por tratar uma única conexão de cliente. Nele, a mensagem de boas-vindas é enviada ao cliente, a resposta é lida e exibida, e a conexão é fechada.
4. O procedimento `Aguardar_Conexoes` é responsável por aguardar novas conexões de clientes. Ele fica em um loop infinito, aceitando conexões e criando uma nova tarefa para processar cada conexão.
5. A função `Criar_Socket_TCP` cria um novo socket TCP/IP, associa-o à porta especificada e coloca-o no modo de escuta.
6. A variável `Servidor` é declarada como um socket TCP/IP criado pela função `Criar_Socket_TCP`.
7. Se o socket for válido, a porta em que o servidor está escutando é exibida na saída do console.
8. O procedimento `Aguardar_Conexoes` é chamado para aguardar conexões de clientes.
9. Quando todas as conexões forem tratadas, o socket do servidor é fechado.

Esse código cria um servidor TCP/IP que recebe conexões de clientes, envia uma mensagem de boas-vindas, recebe uma resposta do cliente e fecha a conexão. É necessário ter o compilador Ada e a biblioteca GNAT.Sockets instalados para compilar e executar o código.