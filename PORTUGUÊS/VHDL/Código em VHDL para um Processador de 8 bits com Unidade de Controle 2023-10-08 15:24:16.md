Claro! Aqui está um exemplo de código complexo em VHDL. Este código descreve um processador simples de 8 bits com uma unidade de controle capaz de executar várias instruções básicas.

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Processador_8bits is
    port (
        clock   : in std_logic;
        reset   : in std_logic;
        opcode  : in std_logic_vector(2 downto 0);
        operand : in std_logic_vector(7 downto 0);
        result  : out std_logic_vector(7 downto 0)
    );
end entity Processador_8bits;

architecture Behavioral of Processador_8bits is
    signal pc      : unsigned(7 downto 0);
    signal regA    : unsigned(7 downto 0);
    signal regB    : unsigned(7 downto 0);
    signal regOut  : unsigned(7 downto 0);
    signal aluOut  : unsigned(7 downto 0);

    type control_t is (
        NOP,
        ADD,
        SUB,
        AND,
        OR,
        XOR,
        LSL,
        LSR,
        JMP,
        JZ,
        JC,
        LD,
        ST,
        HALT
    );
    signal control : control_t;

begin
    process(clock, reset)
    begin
        if reset = '1' then
            pc      <= (others => '0');
            regA    <= (others => '0');
            regB    <= (others => '0');
            regOut  <= (others => '0');
            aluOut  <= (others => '0');
            control <= NOP;
        elsif rising_edge(clock) then
            case control is
                when NOP =>
                    pc <= pc + 1;
                when ADD =>
                    aluOut <= regA + regB;
                    pc <= pc + 1;
                when SUB =>
                    aluOut <= regA - regB;
                    pc <= pc + 1;
                when AND =>
                    aluOut <= regA and regB;
                    pc <= pc + 1;
                when OR =>
                    aluOut <= regA or regB;
                    pc <= pc + 1;
                when XOR =>
                    aluOut <= regA xor regB;
                    pc <= pc + 1;
                when LSL =>
                    aluOut <= regA sll to_integer(unsigned(regB(2 downto 0)));
                    pc <= pc + 1;
                when LSR =>
                    aluOut <= regA srl to_integer(unsigned(regB(2 downto 0)));
                    pc <= pc + 1;
                when JMP =>
                    pc <= to_integer(unsigned(operand));
                when JZ =>
                    if aluOut = 0 then
                        pc <= to_integer(unsigned(operand));
                    else
                        pc <= pc + 1;
                    end if;
                when JC =>
                    if aluOut > 255 then
                        pc <= to_integer(unsigned(operand));
                    else
                        pc <= pc + 1;
                    end if;
                when LD =>
                    regOut <= operand;
                    pc <= pc + 1;
                when ST =>
                    result <= std_logic_vector(regOut);
                    pc <= pc + 1;
                when HALT =>
                    pc <= pc;
            end case;
        end if;
    end process;

    process(opcode)
    begin
        case opcode is
            when "000" => control <= NOP;
            when "001" => control <= ADD;
            when "010" => control <= SUB;
            when "011" => control <= AND;
            when "100" => control <= OR;
            when "101" => control <= XOR;
            when "110" => control <= LSL;
            when "111" => control <= LSR;
            when others => control <= NOP;
        end case;
    end process;

    process(control, operand)
    begin
        case control is
            when LD =>
                regOut <= unsigned(operand);
            when others =>
                regOut <= (others => '0');
        end case;
    end process;

    process(pc, result)
    begin
        case pc is
            when 0 =>
                regA <= "00000000";
                regB <= "00000001";
            when 1 =>
                regA <= "00000010";
                regB <= "00000011";
            when 2 =>
                regA <= "00000100";
                regB <= "00000101";
            when 3 =>
                regA <= "00000110";
                regB <= "00000111";
            when others =>
                regA <= "00000000";
                regB <= "00000000";
        end case;
    end process;
end architecture Behavioral;
```

Este código descreve um processador de 8 bits com uma unidade de controle que suporta várias operações, como adição, subtração, operações lógicas, deslocamentos e transferência de dados. O processador possui um registrador de programa (PC), dois registradores de dados (regA e regB), um registrador de saída (regOut) e um registrador de resultado da ALU (aluOut).

A unidade de controle é responsável por decodificar o opcode recebido e determinar qual operação deve ser executada. As instruções são buscadas na memória sequencialmente, começando do endereço 0. Cada instrução é executada em uma etapa do clock.

Este código é apenas um exemplo básico de um processador em VHDL. Ele pode ser expandido e personalizado para atender às necessidades específicas de um sistema mais complexo.