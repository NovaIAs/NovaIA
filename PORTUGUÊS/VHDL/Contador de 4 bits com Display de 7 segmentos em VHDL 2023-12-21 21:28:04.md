Claro! Aqui está um exemplo de código em VHDL que implementa um contador de 4 bits com saída em display de 7 segmentos:

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity contador_display is
    port(
        clock : in std_logic;
        reset : in std_logic;
        display : out std_logic_vector(6 downto 0)
    );
end entity contador_display;

architecture rtl of contador_display is
    signal contador : unsigned(3 downto 0);
    signal segmentos : std_logic_vector(6 downto 0);
    
    constant tabela_segmentos : std_logic_vector(15 downto 0) := 
        "0000001", -- 0
        "1001111", -- 1
        "0010010", -- 2
        "0000110", -- 3
        "1001100", -- 4
        "0100100", -- 5
        "0100000", -- 6
        "0001111", -- 7
        "0000000", -- 8
        "0001100", -- 9
        "0001000", -- A
        "1100000", -- B
        "0110001", -- C
        "1000010", -- D
        "0110000", -- E
        "0111000"  -- F
    );
begin
    process(clock, reset)
    begin
        if reset = '1' then
            contador <= (others => '0');
            segmentos <= tabela_segmentos(to_integer(contador));
        elsif rising_edge(clock) then
            contador <= contador + 1;
            segmentos <= tabela_segmentos(to_integer(contador));
        end if;
    end process;
    
    display <= segmentos;
    
end architecture rtl;
```

Explicações:
- O código começa com a declaração da biblioteca `ieee` e o uso das diretivas `std_logic_1164` e `numeric_std`, que são necessárias para utilizar os tipos de dados e funções do VHDL.
- A entidade `contador_display` é definida com três portas: `clock` (entrada de clock), `reset` (entrada de reset) e `display` (saída para os segmentos do display de 7 segmentos).
- A arquitetura `rtl` (Register-Transfer Level) é onde o código real é implementado.
- Dentro da arquitetura, são declarados dois sinais: `contador`, um vetor não assinado de 4 bits que armazena o valor do contador, e `segmentos`, um vetor de 7 bits que representa os segmentos do display.
- É definida uma constante `tabela_segmentos` que contém a tabela de segmentos para cada dígito hexadecimal de 0 a F.
- Em seguida, há um processo que é sensível aos sinais `clock` e `reset`. Dentro desse processo, a lógica do contador é implementada.
- Se o sinal `reset` for igual a '1', o contador é resetado para zero e os segmentos são atualizados de acordo com o valor atual do contador.
- Se ocorrer uma borda de subida no sinal `clock`, o contador é incrementado em 1 e os segmentos são atualizados de acordo com o novo valor do contador.
- Por fim, o sinal `display` é atribuído com o valor atual de `segmentos`, exibindo assim os segmentos apropriados no display de 7 segmentos.