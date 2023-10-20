module contador (input clk, input reset, output reg [3:0] count);

 always @(posedge clk or posedge reset)
 begin
   if (reset)
     count <= 4'b0000;
   else
     count <= count + 1;
 end

endmodule

Neste exemplo, temos um módulo em Verilog chamado "contador". Ele possui três sinais: clk (sinal de clock), reset (sinal de reset) e count (registrador de 4 bits que será utilizado como contador).

Dentro do módulo, temos uma always block que é ativada na borda de subida do sinal de clock (posedge clk) ou na borda de subida do sinal de reset (posedge reset). Esta always block é responsável por atualizar o valor do contador.

Dentro da always block, utilizamos uma estrutura condicional if-else para verificar se o sinal de reset está ativo. Se estiver, o valor do contador é reiniciado para 0 (4'b0000). Caso contrário, somamos 1 ao valor atual do contador (count <= count + 1).

Portanto, este código representa um contador que incrementa seu valor a cada ciclo de clock, mas pode ser reiniciado para 0 através do sinal de reset.