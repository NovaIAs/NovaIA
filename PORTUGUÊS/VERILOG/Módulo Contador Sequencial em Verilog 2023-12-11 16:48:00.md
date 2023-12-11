module contador_sequencial (
  input wire clk,
  input wire reset,
  output wire [7:0] saida
);
  
  reg [7:0] contador;
  
  always @(posedge clk or posedge reset) begin
    if (reset)
      contador <= 8'b0;
    else if (contador == 8'b11111111)
      contador <= 8'b0;
    else
      contador <= contador + 1;
  end
  
  assign saida = contador;
  
endmodule

// Neste código, é implementado um contador sequencial utilizando a linguagem de descrição de hardware Verilog. O módulo "contador_sequencial" possui três entradas: clk (sinal de clock), reset (sinal de reset) e uma saída de 8 bits chamada "saida".

// O contador é representado por uma variável reg de 8 bits chamada "contador". Essa variável será utilizada para armazenar o valor do contador.

// A declaração "always @(posedge clk or posedge reset)" indica que o bloco de código dentro do always será ativado na borda de subida do sinal de clock ou na borda de subida do sinal de reset.

// Dentro do bloco always, há uma estrutura condicional if-else. Se o sinal de reset estiver ativo, o valor do contador será resetado para 0. Caso contrário, se o contador estiver no valor máximo (255 em binário), ele voltará para 0. Caso contrário, o valor do contador será incrementado em 1.

// A atribuição "assign saida = contador" faz a conexão entre a variável contador e a saída do módulo.

// Esse código é um exemplo simples de um contador sequencial em Verilog, que conta de 0 a 255 e retorna ao valor 0. Ele pode ser utilizado, por exemplo, em aplicações como contadores de tempo ou geradores de sequências.