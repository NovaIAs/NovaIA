module contador(
  input clk,
  input reset,
  output reg[7:0] contagem,
  output reg zerou
);

  reg[7:0] contador_interno;
  wire[7:0] proxima_contagem;
  
  always @(posedge clk or posedge reset) begin
    if (reset)
      contador_interno <= 8'b0;
    else if (proxima_contagem == 8'b0)
      contador_interno <= 8'b0;
    else
      contador_interno <= proxima_contagem;
  end
  
  assign proxima_contagem = (contador_interno == 8'd9) ? 8'd0 : contador_interno + 1;
  
  always @(posedge clk or posedge reset) begin
    if (reset)
      zerou <= 1'b0;
    else if (proxima_contagem == 8'b0)
      zerou <= 1'b1;
    else
      zerou <= 1'b0;
  end
  
  always @(posedge clk) begin
    if (reset)
      contagem <= 8'b0;
    else if (proxima_contagem == 8'b0)
      contagem <= 8'b0;
    else
      contagem <= proxima_contagem;
  end
  
endmodule

// O código acima implementa um contador síncrono em Verilog. O contador tem uma entrada de clock (clk), uma entrada de reset (reset), uma saída para a contagem (contagem) e uma saída para indicar quando a contagem zerou (zerou).

// O contador interno (contador_interno) é uma variável de 8 bits que armazena a contagem atual. Ele é atualizado a cada ciclo de clock, exceto quando a próxima contagem é zero.

// A próxima contagem (proxima_contagem) é um sinal de 8 bits que indica qual será a próxima contagem. Se o contador interno for igual a 9, a próxima contagem será zero, caso contrário, a próxima contagem será o valor do contador interno mais um.

// O sinal zerou é definido como 1 quando a próxima contagem é zero, indicando que a contagem zerou. Caso contrário, o sinal zerou é definido como 0.

// A saída contagem é atualizada a cada ciclo de clock para refletir a próxima contagem. Se a próxima contagem for zero, a contagem é resetada para zero. Caso contrário, a contagem é atualizada com o valor da próxima contagem.