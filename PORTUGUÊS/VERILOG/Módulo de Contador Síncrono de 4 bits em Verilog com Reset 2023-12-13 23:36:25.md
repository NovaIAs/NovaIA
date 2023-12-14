module contador (
    input clk,      // Sinal de clock
    input reset,    // Sinal de reset
    output reg [3:0] count    // Contador de 4 bits
    );
    
    always @(posedge clk or posedge reset) begin
        if (reset) begin
            count <= 0;    // Reseta o contador quando o sinal de reset é ativado
        end else begin
            if (count == 9) begin
                count <= 0;    // Reinicia o contador quando atinge o valor 9
            end else begin
                count <= count + 1;    // Incrementa o contador
            end
        end
    end
    
endmodule

// Este código implementa um contador síncrono de 4 bits em Verilog. O contador é ativado pelo sinal de clock (clk) e possui um sinal de reset (reset) para reiniciar o contador para zero. 

// O contador é implementado utilizando um registrador de 4 bits (count) que armazena o valor atual do contador. A cada ciclo de clock, o valor do contador é incrementado em 1, a menos que o valor atual seja 9, nesse caso, o contador é reiniciado para zero. 

// O contador é projetado para funcionar corretamente quando o sinal de reset é ativado em qualquer momento, reiniciando instantaneamente o contador para zero.