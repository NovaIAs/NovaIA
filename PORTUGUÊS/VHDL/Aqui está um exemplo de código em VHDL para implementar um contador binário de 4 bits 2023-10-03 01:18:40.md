Claro! Aqui está um exemplo de código em VHDL para implementar um contador binário de 4 bits que faz a contagem de 0 a 15:

```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity contador_binario is
    port (
        clk     : in  std_logic;
        reset   : in  std_logic;
        enable  : in  std_logic;
        count   : out std_logic_vector(3 downto 0)
    );
end contador_binario;

architecture behavioral of contador_binario is
    signal internal_count : std_logic_vector(3 downto 0);
begin
    process(clk, reset)
    begin
        if reset = '1' then
            internal_count <= (others => '0');  -- Reseta o contador para 0
        elsif rising_edge(clk) then
            if enable = '1' then
                internal_count <= internal_count + 1;  -- Incrementa o contador
            end if;
        end if;
    end process;

    count <= internal_count;
end behavioral;
```

Explicação do código:

A primeira parte do código é a declaração da entidade `contador_binario`, que define as portas de entrada (`clk`, `reset` e `enable`) e a porta de saída (`count`), todas do tipo `std_logic`. Essas portas são usadas para controlar e obter os valores do contador binário.

Em seguida, temos a arquitetura `behavioral`, onde o comportamento do contador é definido. Dentro dessa arquitetura, declaramos um sinal interno chamado `internal_count`, que é uma `std_logic_vector` de 4 bits e será usado para armazenar o valor atual do contador.

Dentro do processo sensível aos sinais `clk` e `reset`, verificamos se o sinal `reset` está em nível alto. Se sim, o contador é resetado para o valor 0. Caso contrário, verificamos se houve uma borda de subida (`rising_edge`) no sinal `clk`. Se sim, e se o sinal `enable` também estiver em nível alto, incrementamos o valor do contador `internal_count` em 1.

Por fim, atribuímos o valor do `internal_count` à porta de saída `count`.

Portanto, esse código VHDL implementa um contador binário de 4 bits que incrementa seu valor em 1 a cada pulso de clock, desde que a entrada `enable` esteja em nível alto. O contador é resetado para 0 quando o sinal `reset` está em nível alto. A saída `count` fornece o valor atual do contador em formato binário de 4 bits.