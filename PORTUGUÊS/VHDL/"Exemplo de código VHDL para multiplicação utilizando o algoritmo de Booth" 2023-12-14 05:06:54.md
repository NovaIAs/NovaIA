Claro! Aqui está um exemplo de código em VHDL que realiza a multiplicação de dois números inteiros usando o algoritmo Booth:

```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity multiplicacao_booth is
    generic (
        N : natural := 8
    );
    port (
        clk : in std_logic;
        reset : in std_logic;
        multiplicando : in std_logic_vector(N-1 downto 0);
        multiplicador : in std_logic_vector(N-1 downto 0);
        resultado : out std_logic_vector((2*N)-1 downto 0)
    );
end multiplicacao_booth;

architecture rtl of multiplicacao_booth is
    signal a : signed(N downto 0);
    signal s : signed(N-1 downto 0);
    signal p : signed((2*N)-1 downto 0);
    signal q : signed((2*N)-1 downto 0);
    signal contador : integer range 0 to N;
begin
    process(clk, reset)
    begin
        if reset = '1' then
            contador <= 0;
            a <= (others => '0');
            s <= (others => '0');
            p <= (others => '0');
            q <= (others => '0');
            resultado <= (others => '0');
        elsif rising_edge(clk) then
            if contador = N then
                resultado <= p((2*N)-1 downto N);
            else
                q <= p;
                a <= '0' & multiplicando;
                s <= a(N-1 downto 0) + (not multiplicador(N-1) & multiplicador(N-1 downto 0));
                p <= q + s(N-1 downto 0) & '0';
                contador <= contador + 1;
            end if;
        end if;
    end process;
end rtl;
```

Neste código, estamos definindo uma entidade chamada `multiplicacao_booth` que representa um componente de multiplicação utilizando o algoritmo de Booth. O algoritmo recebe dois números inteiros, `multiplicando` e `multiplicador`, e retorna o resultado da multiplicação em `resultado`.

A arquitetura `rtl` implementa a lógica do algoritmo Booth. O processo principal é sensível à borda de subida do sinal de clock `clk` e também ao sinal `reset`. Quando `reset` está em nível alto, todos os sinais são resetados para seus valores iniciais.

Dentro do processo, temos uma estrutura condicional que verifica o valor do contador. Quando o contador atingir o valor N, a multiplicação está completa e o resultado é atribuído à saída `resultado`. Caso contrário, o algoritmo continua calculando os passos intermediários.

Os sinais `a`, `s`, `p` e `q` são sinais intermediários utilizados pelo algoritmo de Booth para realizar os cálculos. O sinal `contador` é utilizado para controlar o fluxo do algoritmo.

Espero que este código atenda às suas necessidades! Se você tiver alguma dúvida ou precisar de mais esclarecimentos, fique à vontade para perguntar.