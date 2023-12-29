module contador_binario (
  input wire clk,
  output wire [7:0] saida
);

  reg [7:0] contador;

  always @(posedge clk) begin
    if (contador == 8'b11111111) begin
      contador <= 8'b00000000;
    end else begin
      contador <= contador + 1;
    end
  end

  assign saida = contador;

endmodule

// Neste código, estamos implementando um contador binário de 8 bits em Verilog. 

// Na declaração do módulo, temos uma entrada de clock (clk) e uma saída de 8 bits (saida).

// A variável contador é declarada como um registrador de 8 bits, que vai armazenar o valor atual do contador.

// Dentro do bloco always @(posedge clk), ou seja, a cada flanco de subida do clock, verificamos se o contador está no valor máximo (11111111). 
// Se estiver, reiniciamos o contador para 00000000. Caso contrário, incrementamos o contador em 1.

// Por fim, atribuímos a saída o valor atual do contador.

// Assim, este código implementa um contador binário de 8 bits que conta de 0 a 255 e reinicia automaticamente.