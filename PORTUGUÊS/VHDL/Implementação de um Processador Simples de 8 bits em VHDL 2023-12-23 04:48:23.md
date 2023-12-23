Claro! Abaixo está um exemplo de código em VHDL que implementa um processador simples de 8 bits. O processador possui uma unidade de controle que executa instruções básicas de manipulação de dados, como adição, subtração, AND, OR e deslocamento.

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity processador is
    port (
        clock       : in  std_logic;
        reset       : in  std_logic;
        opcode      : in  std_logic_vector(3 downto 0);
        operand_a   : in  std_logic_vector(7 downto 0);
        operand_b   : in  std_logic_vector(7 downto 0);
        result      : out std_logic_vector(7 downto 0)
    );
end entity processador;

architecture behavioral of processador is
    signal alu_result    : std_logic_vector(7 downto 0);
    signal temp_result   : std_logic_vector(7 downto 0);
    signal shift_result  : std_logic_vector(7 downto 0);
    
begin
    
    process(clock, reset)
    begin
        if (reset = '1') then
            alu_result <= (others => '0');
            temp_result <= (others => '0');
            shift_result <= (others => '0');
        elsif (rising_edge(clock)) then
            case opcode is
                when "0000" => -- Soma
                    alu_result <= std_logic_vector(unsigned(operand_a) + unsigned(operand_b));
                when "0001" => -- Subtração
                    alu_result <= std_logic_vector(unsigned(operand_a) - unsigned(operand_b));
                when "0010" => -- AND
                    alu_result <= operand_a and operand_b;
                when "0011" => -- OR
                    alu_result <= operand_a or operand_b;
                when "0100" => -- Deslocamento à direita
                    shift_result <= operand_a srl to_integer(unsigned(operand_b));
                when "0101" => -- Deslocamento à esquerda
                    shift_result <= operand_a sll to_integer(unsigned(operand_b));
                when others =>
                    alu_result <= (others => '0');
                    shift_result <= (others => '0');
            end case;
        end if;
    end process;

    process(reset, alu_result, shift_result)
    begin
        if (reset = '1') then
            temp_result <= (others => '0');
        else
            temp_result <= alu_result;
            if (shift_result /= (others => '0')) then
                temp_result <= shift_result;
            end if;
        end if;
    end process;

    result <= temp_result;
    
end architecture behavioral;
```

Neste código, a entidade `processador` possui cinco sinais de entrada: `clock`, `reset`, `opcode`, `operand_a` e `operand_b`. O sinal de saída é chamado de `result`. O processador é síncrono e possui uma unidade de controle implementada no processo interno.

A unidade de controle é ativada na borda de subida do sinal de relógio (`rising_edge(clock)`) e verifica o valor do sinal `opcode`. Dependendo do valor do `opcode`, a unidade de controle executa a operação correspondente, atualizando o sinal `alu_result` com o resultado da operação. As operações suportadas são soma, subtração, AND, OR e deslocamento à esquerda e à direita.

Além disso, há um segundo processo que controla o resultado final. Ele verifica o sinal `shift_result` e, se não for igual a zero, atualiza o sinal `temp_result` com o valor do deslocamento. Caso contrário, o sinal `temp_result` é atualizado com o valor do `alu_result`.

Por fim, o resultado final é atribuído ao sinal de saída `result`.

Espero que este exemplo de código em VHDL atenda às suas expectativas!