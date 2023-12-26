module contador_verilog(
    input wire clk,
    input wire reset,
    output wire[3:0] display
);

reg[3:0] count;

always @(posedge clk or posedge reset) begin
    if (reset) begin
        count <= 4'b0000;
    end else begin
        count <= count + 1;
    end
end

always @(count) begin
    case(count)
        4'b0000: display <= 4'b0000; // exibe 0 no display de 7 segmentos
        4'b0001: display <= 4'b0001; // exibe 1 no display de 7 segmentos
        4'b0010: display <= 4'b0010; // exibe 2 no display de 7 segmentos
        4'b0011: display <= 4'b0011; // exibe 3 no display de 7 segmentos
        4'b0100: display <= 4'b0100; // exibe 4 no display de 7 segmentos
        4'b0101: display <= 4'b0101; // exibe 5 no display de 7 segmentos
        4'b0110: display <= 4'b0110; // exibe 6 no display de 7 segmentos
        4'b0111: display <= 4'b0111; // exibe 7 no display de 7 segmentos
        4'b1000: display <= 4'b1000; // exibe 8 no display de 7 segmentos
        4'b1001: display <= 4'b1001; // exibe 9 no display de 7 segmentos
        default: display <= 4'b0000; // exibe 0 no display de 7 segmentos
    endcase
end

endmodule

Neste código em Verilog, é criado um módulo chamado "contador_verilog" que implementa um contador de 4 bits. O contador incrementa a cada pulso positivo do clock (clk) e reseta para zero quando o sinal de reset é ativado.

O valor atual do contador é armazenado em um registrador (reg[3:0] count) que é atualizado dentro do bloco "always" usando uma declaração "if-else". Se o sinal de reset estiver ativo, o valor do contador é resetado para zero. Caso contrário, o valor do contador é incrementado em 1.

Em seguida, há outro bloco "always" que utiliza o valor do contador para determinar o valor a ser exibido em um display de 7 segmentos. O bloco usa uma declaração "case" para mapear cada valor do contador para o valor correspondente a ser exibido. Por exemplo, se o contador for 4'b0000, o display mostrará o número 0, se o contador for 4'b0001, o display mostrará o número 1, e assim por diante.

Caso o valor do contador não corresponda a nenhum dos casos especificados no bloco "case", o display mostrará o número 0.

Esse código pode ser utilizado para implementar um contador simples em um circuito em FPGA, por exemplo.