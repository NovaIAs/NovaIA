module contador_binario (
  input wire clk,
  input wire reset,
  output wire [3:0] saida
);

  reg [3:0] contador;
  
  always @(posedge clk) begin
    if (reset) begin
      contador <= 4'b0000;
    end else begin
      case (contador)
        4'b0000: contador <= 4'b0001;
        4'b0001: contador <= 4'b0010;
        4'b0010: contador <= 4'b0011;
        4'b0011: contador <= 4'b0100;
        4'b0100: contador <= 4'b0101;
        4'b0101: contador <= 4'b0110;
        4'b0110: contador <= 4'b0111;
        4'b0111: contador <= 4'b1000;
        4'b1000: contador <= 4'b1001;
        4'b1001: contador <= 4'b1010;
        4'b1010: contador <= 4'b1011;
        4'b1011: contador <= 4'b1100;
        4'b1100: contador <= 4'b1101;
        4'b1101: contador <= 4'b1110;
        4'b1110: contador <= 4'b1111;
        4'b1111: contador <= 4'b0000;
      endcase
    end
  end
  
  assign saida = contador;
  
endmodule

// Este código é um módulo Verilog que implementa um contador binário de 4 bits.
// A saída do contador é uma variável de 4 bits chamada "saida".
// O contador incrementa a cada borda de subida do sinal de clock (clk) e pode ser redefinido para 0 quando o sinal de reset (reset) está ativo.
// O contador possui uma sequência de contagem de 0 a 15 em binário.
// O código utiliza uma estrutura "always" sensível à borda de subida do clock para atualizar o valor do contador.
// O valor do contador é armazenado em um registrador (reg [3:0] contador) que é atualizado com base na lógica do bloco "case".
// A atribuição "assign" é usada para conectar a saída do contador com a variável "saida".
// Este código é escrito em português e pode ser usado para implementar um contador binário em FPGA ou outro dispositivo digital.