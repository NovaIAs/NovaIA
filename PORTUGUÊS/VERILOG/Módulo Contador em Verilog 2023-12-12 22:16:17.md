module contador(
    input wire clk,
    input wire reset,
    output wire[7:0] display
);

    reg[7:0] count;

    always @(posedge clk or posedge reset)
    begin
        if (reset)
            count <= 8'b0;
        else
            count <= count + 1;
    end

    always @(count)
    begin
        case (count)
            8'b00000000: display <= 8'b00000001;
            8'b00000001: display <= 8'b00000010;
            8'b00000010: display <= 8'b00000011;
            8'b00000011: display <= 8'b00000100;
            8'b00000100: display <= 8'b00000101;
            8'b00000101: display <= 8'b00000110;
            8'b00000110: display <= 8'b00000111;
            8'b00000111: display <= 8'b00001000;
            8'b00001000: display <= 8'b00001001;
            8'b00001001: display <= 8'b00001010;
            8'b00001010: display <= 8'b00001011;
            8'b00001011: display <= 8'b00001100;
            8'b00001100: display <= 8'b00001101;
            8'b00001101: display <= 8'b00001110;
            8'b00001110: display <= 8'b00001111;
            8'b00001111: display <= 8'b00010000;
            default: display <= 8'b00000000;
        endcase
    end
    
endmodule

/*
    Módulo contador em Verilog.
    
    Este módulo implementa um contador de 8 bits que incrementa a cada pulso de clock.
    O valor do contador é exibido no display de 8 bits.
    O contador é reiniciado para 0 quando o sinal de reset é acionado.

    Entradas:
    - clk: sinal de clock
    - reset: sinal de reset

    Saída:
    - display: valor do contador a ser exibido no display

    Funcionamento:
    O contador é implementado usando um registrador de 8 bits chamado "count".
    Ele incrementa em 1 a cada pulso de clock, a menos que o sinal de reset esteja ativo.
    O valor atual do contador é exibido no display através de uma tabela de casos (case statement)
    que mapeia cada valor do contador para o valor correspondente a ser exibido.

    Nota:
    O código gerado pode parecer simples, mas ele ilustra o funcionamento básico de um contador em Verilog.
    Cada caso no case statement representa um valor do contador e o valor correspondente a ser exibido.
    Você pode modificar o código para adicionar mais casos ou torná-lo mais complexo de acordo com suas necessidades.
*/