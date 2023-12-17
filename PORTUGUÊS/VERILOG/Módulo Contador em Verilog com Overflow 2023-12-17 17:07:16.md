module Contador(
    input clk,
    input reset,
    output reg [7:0] count,
    output reg overflow
);

always @(posedge clk) begin
    if (reset) begin
        count <= 8'b00000000;
        overflow <= 1'b0;
    end else begin
        if (count == 8'b11111111) begin
            count <= 8'b00000000;
            overflow <= 1'b1;
        end else begin
            count <= count + 1;
            overflow <= 1'b0;
        end
    end
end

endmodule

// Módulo principal
module Main;
reg clk;
reg reset;
wire [7:0] count;
wire overflow;

initial begin
    clk = 1'b0;
    reset = 1'b0;

    #5 reset = 1'b1;  // Ativa o reset
    
    #10 reset = 1'b0; // Desativa o reset
    
    #100 $finish;    // Encerra a simulação
end

always begin
    #5 clk = ~clk;   // Inverte o sinal do clock a cada 5 unidades de tempo
end

Contador U1(
    .clk(clk),
    .reset(reset),
    .count(count),
    .overflow(overflow)
);

initial begin
    $monitor("Tempo: %t, Contador: %h, Overflow: %b", $time, count, overflow);
end

endmodule
O código acima é um exemplo de um contador de 8 bits implementado em Verilog. Ele utiliza um clock para incrementar um valor de contagem a cada ciclo de clock. Quando o valor de contagem atinge o máximo (255 em binário), o contador é reiniciado para zero e um sinal de overflow é ativado.

O módulo "Contador" contém a lógica do contador. Ele possui entradas para o clock e para um sinal de reset, que é usado para reiniciar o contador. Também possui duas saídas: uma para o valor de contagem de 8 bits e outra para o sinal de overflow.

Dentro do bloco "always @(posedge clk)", o código verifica se o sinal de reset está ativo. Se estiver, o contador é reiniciado para zero e o sinal de overflow é desativado. Caso contrário, o código verifica se o valor de contagem atingiu o máximo. Se sim, o contador é reiniciado para zero e o sinal de overflow é ativado. Caso contrário, o valor de contagem é incrementado em 1 e o sinal de overflow é desativado.

No módulo principal "Main", são declarados os sinais de clock e reset como registradores (reg). Em seguida, é inicializada a simulação, configurando o sinal de reset para ativo durante 5 unidades de tempo e, em seguida, desativado. O clock é alternado a cada 5 unidades de tempo.

O módulo "Contador" é instanciado como U1, passando os sinais de clock, reset, count e overflow como argumentos.

Por fim, é definido um bloco initial para exibir na tela o tempo de simulação, o valor de contagem hexadecimal e o sinal de overflow. A simulação é finalizada após 100 unidades de tempo.