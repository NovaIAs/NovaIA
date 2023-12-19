module contador (
  input wire clk,
  output wire [3:0] cont_out
);

  reg [3:0] contador_reg;

  always @(posedge clk) begin
    if (contador_reg == 4'b1111)
      contador_reg <= 4'b0000;
    else
      contador_reg <= contador_reg + 1;
  end

  assign cont_out = contador_reg;

endmodule

// Neste código, estou criando um módulo Verilog chamado "contador". Este módulo recebe um sinal de relógio (clk) e possui uma saída de 4 bits (cont_out).

// Dentro do módulo, tenho uma variável registradora (reg) chamada "contador_reg", que armazena o valor atual do contador. 

// A funcionalidade principal do código está no bloco "always @(posedge clk)", que é ativado a cada borda de subida do sinal de relógio. Dentro deste bloco, faço uma verificação utilizando uma estrutura condicional "if-else".

// Se o valor atual do contador_reg for igual a 15 (binário 1111), significa que o contador chegou ao seu valor máximo. Nesse caso, o contador_reg é resetado para 0 (binário 0000) utilizando a atribuição "contador_reg <= 4'b0000".

// Caso contrário, o contador_reg é incrementado em 1 utilizando a atribuição "contador_reg <= contador_reg + 1".

// Por fim, a saída cont_out é atribuída ao valor atual do contador_reg utilizando a declaração "assign cont_out = contador_reg".

// Esse código representa um contador de 4 bits que conta de 0 a 15, reiniciando quando chega ao valor máximo.