module contador (clk, reset, enable, saida);

input clk, reset, enable;
output reg [31:0] saida;

reg [31:0] contador_interno;
reg [1:0] estado;

parameter IDLE = 2'b00;
parameter COUNT = 2'b01;
parameter RESET = 2'b10;

always @(posedge clk) begin
    if (reset) begin
        estado <= RESET;
        contador_interno <= 32'h00000000;
        saida <= 32'h00000000;
    end else begin
        case (estado)
            IDLE: begin
                if (enable)
                    estado <= COUNT;
            end
            
            COUNT: begin
                contador_interno <= contador_interno + 1;
                if (contador_interno == 32'hFFFFFFFF)
                    estado <= IDLE;
            end
            
            RESET: begin
                estado <= IDLE;
            end
        endcase
    end
end

assign saida = contador_interno;

endmodule

//Explicação do código:
//Este código em Verilog implementa um contador de 32 bits que conta de 0 a 4.294.967.295 (2^32 - 1).
//O módulo "contador" possui quatro entradas (clk, reset, enable) e uma saída (saida).
//A entrada "clk" é o clock do sistema, a entrada "reset" é um sinal de reset assíncrono e a entrada "enable" é um sinal de habilitação.
//A saída "saida" é o valor atual do contador de 32 bits.
//O módulo utiliza registradores (reg) para armazenar o estado atual do contador (estado) e o valor atual do contador interno (contador_interno).
//O contador é implementado utilizando uma máquina de estados com três estados: IDLE, COUNT e RESET.
//No estado IDLE, o contador está esperando o sinal de habilitação (enable) para iniciar a contagem.
//Quando o sinal de habilitação é ativado, o contador muda para o estado COUNT e começa a incrementar o valor do contador interno a cada pulso de clock (posedge clk).
//Quando o valor do contador interno atinge o valor máximo (32'hFFFFFFFF), o contador muda para o estado IDLE e aguarda o próximo sinal de habilitação para reiniciar a contagem.
//No estado RESET, o contador é redefinido para o valor inicial (32'h00000000).
//A saída do contador é atualizada a cada pulso de clock e reflete o valor atual do contador interno.
//Este código é um exemplo de implementação básica de um contador em Verilog. Pode ser utilizado em diferentes aplicações que requerem contagem de eventos ou temporização.