Claro! Aqui está um exemplo de um código complexo em ADA:

```
with Ada.Text_IO; use Ada.Text_IO;

procedure Complex_Code is

   -- Definindo um tipo de dado personalizado
   type Meu_Tipo is range 1 .. 10;

   -- Definindo um tipo de dado enumerado
   type Estado is (Inativo, Ativo, Pausado);

   -- Definindo um registro com múltiplos campos
   type Registro is record
      Nome       : String (1 .. 50);
      Idade      : Integer range 0 .. 150;
      Altura     : Float range 0.0 .. 3.0;
      Ativo      : Boolean;
      Estado_Civil : Estado;
   end record;

   -- Declarando uma variável do tipo Registro
   Minha_Pessoa : Registro;

   -- Procedimento para exibir informações do registro
   procedure Exibir_Info (Pessoa : in Registro) is
   begin
      Put_Line("Nome: " & Pessoa.Nome);
      Put_Line("Idade: " & Integer'Image(Pessoa.Idade));
      Put_Line("Altura: " & Float'Image(Pessoa.Altura));
      Put_Line("Ativo: " & Boolean'Image(Pessoa.Ativo));
      case Pessoa.Estado_Civil is
         when Inativo =>
            Put_Line("Estado Civil: Inativo");
         when Ativo =>
            Put_Line("Estado Civil: Ativo");
         when Pausado =>
            Put_Line("Estado Civil: Pausado");
      end case;
   end Exibir_Info;

   -- Função para calcular o quadrado de um número
   function Quadrado (Numero : in Integer) return Integer is
   begin
      return Numero * Numero;
   end Quadrado;

begin
   -- Preenchendo os campos do registro
   Minha_Pessoa.Nome := "João";
   Minha_Pessoa.Idade := 25;
   Minha_Pessoa.Altura := 1.75;
   Minha_Pessoa.Ativo := True;
   Minha_Pessoa.Estado_Civil := Ativo;

   -- Chamando o procedimento para exibir informações
   Exibir_Info(Minha_Pessoa);

   -- Calculando e exibindo o quadrado da idade
   Put_Line("Quadrado da Idade: " & Integer'Image(Quadrado(Minha_Pessoa.Idade)));
end Complex_Code;
```

Neste exemplo, criamos um código em ADA que demonstra a utilização de tipos de dados personalizados, um tipo enumerado, registros e procedimentos. O programa cria uma variável do tipo Registro chamada "Minha_Pessoa" e preenche seus campos com informações como nome, idade, altura, estado civil e um booleano indicando se a pessoa está ativa ou não.

Em seguida, temos um procedimento chamado "Exibir_Info", que recebe um parâmetro do tipo Registro e exibe suas informações na tela. Utilizamos uma estrutura case para exibir o estado civil de acordo com os valores possíveis do tipo enumerado "Estado".

Também temos uma função chamada "Quadrado" que recebe um número inteiro e retorna o seu quadrado. Neste exemplo, chamamos essa função para calcular o quadrado da idade da pessoa e exibimos o resultado na tela.

Espero que este código complexo em ADA possa lhe proporcionar uma compreensão maior sobre a linguagem e suas funcionalidades!
