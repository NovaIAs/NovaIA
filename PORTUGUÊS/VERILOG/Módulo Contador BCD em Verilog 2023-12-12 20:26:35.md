module contador_bcd(
    input wire clk,
    input wire reset,
    output wire [3:0] bcd
);

reg [3:0] contador;

always @(posedge clk or posedge reset) begin
    if (reset)
        contador <= 4'b0000;
    else begin
        case (contador)
            4'b0000: contador <= 4'b0001;
            4'b0001: contador <= 4'b0010;
            4'b0010: contador <= 4'b0011;
            4'b0011: contador <= 4'b0100;
            4'b0100: contador <= 4'b0101;
            4'b0101: contador <= 4'b0110;
            4'b0110: contador <= 4'b0111;
            4'b0111: contador <= 4'b1000;
            4'b1000: contador <= 4'b1001;
            4'b1001: contador <= 4'b0000;
            default: contador <= 4'b0000;
        endcase
    end
end

assign bcd = contador;

endmodule
```

Este é um código em Verilog que implementa um contador BCD de 4 bits. 

O módulo `contador_bcd` possui três portas: `clk`, `reset` e `bcd`. O sinal de `clk` é a entrada do clock usado para sincronizar o contador. O sinal de `reset` é a entrada de reset assíncrono que zera o contador quando ativado. O sinal de `bcd` é a saída do contador em formato BCD de 4 bits.

Dentro do bloco `always @(posedge clk or posedge reset)` é onde a lógica de contagem acontece. Sempre que ocorre uma borda de subida no sinal de `clk` ou uma borda de subida no sinal de `reset`, o bloco é executado. Se o sinal de `reset` estiver em nível alto, o contador é resetado para o valor 4'b0000. Caso contrário, o contador é incrementado de acordo com a sequência de contagem BCD (de 0 a 9) usando um bloco `case`.

A instrução `assign bcd = contador;` atribui o valor do contador à saída `bcd`. 

Esse código é útil para implementar contadores BCD em circuitos digitais, onde a contagem precisa ser exibida em um formato decimal de 4 dígitos.