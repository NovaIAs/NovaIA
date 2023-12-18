module contador (
  input wire clk,
  input wire reset,
  output wire[31:0] count
);

  reg[31:0] contador;

  always @(posedge clk or posedge reset) begin
    if (reset)
      contador <= 0;
    else if (contador == 1000000000)
      contador <= 0;
    else
      contador <= contador + 1;
  end

  assign count = contador;

endmodule

// Neste código, estamos implementando um contador em Verilog que conta até 1 bilhão e depois reinicia a contagem. O contador é incrementado a cada ciclo de clock (borda de subida) e também pode ser reiniciado manualmente com um sinal de reset.

// O módulo "contador" possui três portas: "clk" (entrada de clock), "reset" (entrada de reset) e "count" (saída do contador).

// A variável "contador" é um registrador de 32 bits que armazena o valor atual do contador. 

// A instrução "always @(posedge clk or posedge reset)" é uma construção que indica que o bloco de código dentro dele deve ser executado sempre que houver uma borda de subida no sinal de clock ou no sinal de reset.

// Dentro do bloco always, há uma estrutura condicional que verifica se o sinal de reset está ativo. Se estiver, o contador é reiniciado para zero. Caso contrário, o código verifica se o contador atingiu o valor de 1 bilhão (1000000000). Se sim, o contador é reiniciado para zero novamente. Caso contrário, o contador é incrementado em 1.

// A instrução "assign count = contador;" atribui o valor atual do contador à saída "count". Assim, podemos obter o valor do contador externamente ao módulo.