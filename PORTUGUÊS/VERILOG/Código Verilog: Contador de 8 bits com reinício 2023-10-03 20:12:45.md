Claro! Aqui está um exemplo de código em Verilog que cria um contador de 8 bits que incrementa de 0 até 255 e depois reinicia para 0:

```verilog
module contador (
  input wire clk,
  input wire reset,
  output wire [7:0] count
);

  reg [7:0] count_reg;

  always @(posedge clk) begin
    if (reset) begin
      count_reg <= 8'b00000000;
    end else begin
      if (count_reg == 8'b11111111) begin
        count_reg <= 8'b00000000;
      end else begin
        count_reg <= count_reg + 1;
      end
    end
  end

  assign count = count_reg;

endmodule
```

Neste código, definimos um módulo chamado "contador" com três portas: "clk" para o sinal de clock, "reset" para o sinal de reset e "count" para o valor do contador de 8 bits.

Dentro do módulo, declaramos um registrador de 8 bits chamado "count_reg" que armazenará o valor atual do contador.

Em seguida, usamos um bloco "always @(posedge clk)" para sincronizar a lógica do contador com a borda de subida do sinal de clock. Dentro do bloco "always", verificamos se o sinal de reset está ativo. Se estiver, o contador é reiniciado para 0. Caso contrário, verificamos se o valor atual do contador é 255 (8'b11111111). Se sim, o contador é reiniciado para 0. Caso contrário, o valor do contador é incrementado em 1.

Por fim, usamos a declaração "assign" para atribuir o valor do registrador "count_reg" à saída "count".

Esse código cria um contador de 8 bits que incrementa de 0 até 255 e depois reinicia para 0, sempre que o sinal de reset estiver ativo.