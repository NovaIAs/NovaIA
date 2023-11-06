module contador(
  input wire clk,
  input wire rst,
  output wire [7:0] cont
);

  reg [7:0] count;

  always @(posedge clk or posedge rst) begin
    if (rst)
      count <= 8'b00000000;
    else if (count == 8'b11111111)
      count <= 8'b00000000;
    else
      count <= count + 1;
  end

  assign cont = count;

endmodule

Neste código em Verilog, estamos criando um módulo chamado "contador" com três portas: clk (sinal de clock), rst (sinal de reset) e cont (contador de 8 bits).

Dentro do módulo, declaramos um registrador count de 8 bits, responsável por armazenar o valor do contador.

A parte principal do código está dentro do bloco always, que é sensível à borda de subida do sinal de clock (posedge clk) e à borda de subida do sinal de reset (posedge rst). Isso significa que a lógica dentro do bloco always será executada a cada ciclo de clock ou quando há um evento de reset.

Dentro do bloco always, temos uma estrutura condicional. Se o sinal de reset rst estiver ativo (em nível alto), o contador é reiniciado para 8'b00000000 (valor inicial). Caso contrário, se o contador atingir o valor máximo de 8'b11111111, ele será reiniciado para 8'b00000000. Caso contrário, o contador incrementa seu valor atual em 1.

Por fim, utilizamos a atribuição assign para conectar o valor do contador cont ao registrador count.

Este código cria um contador de 8 bits, que incrementa de 0 a 255 e reinicia para 0.