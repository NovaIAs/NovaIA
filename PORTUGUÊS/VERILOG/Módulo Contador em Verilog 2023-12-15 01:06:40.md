module contador (
    input clk,
    input rst,
    output reg [7:0] count
    );
 
 reg [7:0] temp;
 
 always @(posedge clk or posedge rst) begin
    if (rst) begin
       temp <= 8'b00000000;
    end else begin
       if (temp == 8'b11111111) begin
          temp <= 8'b00000000;
       end else begin
          temp <= temp + 1;
       end
    end
 end
 
 always @(posedge clk) begin
    count <= temp;
 end
endmodule

// Módulo contador

// Este módulo é um contador de 8 bits que será usado para contar os ciclos de clock.
// Ele possui uma entrada para o sinal de clock (clk), uma entrada para o sinal de reset (rst) e uma saída para o valor do contador (count).

// O sinal de clock é usado para sincronizar as operações do contador. Quando ocorre uma borda de subida no sinal de clock, o valor atual do contador é atualizado.

// O sinal de reset é usado para reiniciar o contador. Quando ocorre uma borda de subida no sinal de reset, o valor do contador é redefinido para 0.

// O valor do contador é armazenado em um registrador chamado temp. Inicialmente, o valor de temp é definido como 0.

// Sempre que ocorre uma borda de subida no sinal de clock ou no sinal de reset, o bloco always é executado.

// Se o sinal de reset estiver em nível alto, o valor do registrador temp é definido como 0.

// Caso contrário, se o valor de temp for igual a 255 (11111111 em binário), o valor de temp é redefinido para 0.

// Caso contrário, o valor de temp é incrementado em 1.

// Sempre que ocorre uma borda de subida no sinal de clock, o valor de count é atualizado com o valor de temp.