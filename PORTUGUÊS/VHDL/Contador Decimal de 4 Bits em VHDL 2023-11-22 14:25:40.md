Claro! Aqui está um código em VHDL que implementa um contador decimal de 4 bits.

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity contador_4bits is
    port(
        clock : in std_logic;
        reset : in std_logic;
        enable : in std_logic;
        saida : out std_logic_vector(3 downto 0)
    );
end entity contador_4bits;

architecture rtl of contador_4bits is
    signal contador : unsigned(3 downto 0);
begin
    process(clock, reset)
    begin
        if reset = '1' then
            contador <= (others => '0');
        elsif rising_edge(clock) then
            if enable = '1' then
                contador <= contador + 1;
            end if;
        end if;
    end process;

    saida <= std_logic_vector(contador);
end architecture rtl;
```

Explicação do código:

1. A biblioteca `ieee` é utilizada para importar o pacote `std_logic_1164` e o pacote `numeric_std`, que são padrões no VHDL para lidar com sinais digitais e operações numéricas respectivamente.

2. A entidade `contador_4bits` define as interfaces do nosso contador, que são:
   - `clock` : sinal de clock que sincroniza o funcionamento do contador.
   - `reset` : sinal de reset que zera o valor do contador.
   - `enable` : sinal de habilitação que controla se o contador deve incrementar ou não.
   - `saida` : sinal de saída que representa o valor atual do contador.

3. A arquitetura `rtl` é onde o comportamento interno do contador é especificado.

4. Dentro da arquitetura, declaramos um sinal chamado `contador` do tipo `unsigned` com tamanho de 4 bits (3 downto 0).

5. O processo `process(clock, reset)` é responsável por atualizar o valor do contador.

6. Se o sinal `reset` for '1', o contador é resetado para zero.

7. Caso contrário, se houver uma borda de subida no sinal `clock` e o sinal `enable` for '1', o contador é incrementado em 1.

8. A instrução `saida <= std_logic_vector(contador)` converte o valor do contador para um sinal de saída do tipo `std_logic_vector` e o atribui à saída do contador.

Esse código implementa um contador decimal simples de 4 bits, em que a cada pulso de clock, o valor é incrementado em 1, desde que o sinal `enable` esteja ativo.