module contador (
  input wire clk,
  input wire rst,
  output wire [31:0] saida
  );
  
  reg [31:0] contador;

  always @(posedge clk or posedge rst)
  begin
    if (rst)
      contador <= 32'b0;
    else
      contador <= contador + 1;
  end

  assign saida = contador;

endmodule

// Neste código, estamos criando um módulo chamado "contador" que tem três entradas e uma saída. As entradas são um clock (clk), um sinal de reset (rst) e a saída é um vetor de 32 bits chamado "saida".

// Definimos um registro (reg) chamado "contador" com 32 bits para armazenar o valor do contador.

// Usamos a instrução "always @(posedge clk or posedge rst)" para indicar que o bloco de código dentro dele deve ser executado sempre que houver uma transição de nível positivo no sinal de clk ou uma transição de nível positivo no sinal de reset.

// Dentro do bloco always, temos uma estrutura condicional onde verificamos se o sinal de reset está ativo (rst). Se estiver, o valor do contador é resetado para zero. Caso contrário, o valor do contador é incrementado em uma unidade.

// Por fim, fazemos a atribuição do valor do contador à saída do módulo usando a instrução "assign saida = contador".

// Esse código é um exemplo de um contador síncrono de 32 bits em Verilog. Ele conta de 0 a 2^32-1 e reinicia a contagem quando o sinal de reset é ativado. O valor atual do contador é sempre refletido na saída do módulo.