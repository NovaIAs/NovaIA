module contador (
    input clk,
    input reset,
    output reg [7:0] cont
);

reg [7:0] cont_temp;

always @(posedge clk or posedge reset)
begin
    if (reset)
        cont_temp <= 8'b00000000;
    else
        cont_temp <= cont_temp + 1;
end

always @(posedge clk)
begin
    if (reset)
        cont <= 8'b00000000;
    else
        cont <= cont_temp;
end

endmodule

Este é um código em Verilog para implementar um contador de 8 bits. O contador incrementa seu valor em 1 a cada pulso de clock. Quando o sinal de reset é ativado, o contador é resetado para 0.

O módulo "contador" tem três portas: clk (o sinal de clock), reset (o sinal de reset) e cont (o valor de saída do contador).

Dentro do módulo, temos duas variáveis registráveis (reg): cont_temp e cont. A variável cont_temp é usada para armazenar temporariamente o próximo valor do contador antes de ser atualizado, enquanto a variável cont armazena o valor atual do contador.

A primeira always block é sensível a borda de subida do sinal de clock e ao sinal de reset. No bloco always, verificamos se o sinal de reset está ativo. Se sim, atribuímos a cont_temp o valor '00000000' para resetar o contador. Caso contrário, incrementamos cont_temp em 1.

A segunda always block é sensível à borda de subida do sinal de clock. No bloco always, verificamos se o sinal de reset está ativo. Se sim, atribuímos a cont o valor '00000000' para resetar o contador. Caso contrário, atribuímos a cont o valor de cont_temp, atualizando assim o valor do contador.

Esse código permite que você implemente um contador de 8 bits em um dispositivo programável, como um FPGA, por exemplo. Ele pode ser utilizado em diversas aplicações, como controle de tempo, contagem de eventos, entre outros.