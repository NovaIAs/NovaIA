Aqui está um código em VHDL complexo que cria um processador de 8 bits capaz de executar operações básicas de soma, subtração e multiplicação. Ele possui uma arquitetura pipeline de três estágios: busca de instrução, decodificação e execução.

```vhdl
-- Definindo a entidade do processador
entity Processor is
    Port (
        clk : in std_logic;
        reset : in std_logic;
        input_a : in std_logic_vector(7 downto 0);
        input_b : in std_logic_vector(7 downto 0);
        opcode : in std_logic_vector(2 downto 0);
        output_result : out std_logic_vector(7 downto 0);
        output_valid : out std_logic
    );
end Processor;

architecture Behavioral of Processor is
    -- Definindo os sinais internos do processador
    signal pc : std_logic_vector(7 downto 0);
    signal instruction : std_logic_vector(23 downto 0);
    signal operand_a : std_logic_vector(7 downto 0);
    signal operand_b : std_logic_vector(7 downto 0);
    signal result : std_logic_vector(7 downto 0);
    
begin
    -- Módulo de busca de instrução
    process(clk, reset)
    begin
        if reset = '1' then
            pc <= (others => '0');
        elsif rising_edge(clk) then
            if pc = "11111111" then
                pc <= (others => '0');
            else
                pc <= std_logic_vector(unsigned(pc) + 1);
            end if;
        end if;
    end process;
    
    -- Módulo de decodificação
    process(clk, reset, pc, opcode, input_a, input_b)
    begin
        if reset = '1' then
            instruction <= (others => '0');
            operand_a <= (others => '0');
            operand_b <= (others => '0');
        elsif rising_edge(clk) then
            instruction <= input_a & input_b & opcode;
            operand_a <= input_a;
            operand_b <= input_b;
        end if;
    end process;
    
    -- Módulo de execução
    process(clk, reset, instruction, operand_a, operand_b)
    begin
        if reset = '1' then
            result <= (others => '0');
            output_valid <= '0';
        elsif rising_edge(clk) then
            case instruction is
                when "0000" & opcode =>
                    result <= operand_a + operand_b;
                    output_valid <= '1';
                when "0001" & opcode =>
                    result <= operand_a - operand_b;
                    output_valid <= '1';
                when "0010" & opcode =>
                    result <= operand_a * operand_b;
                    output_valid <= '1';
                when others =>
                    result <= (others => '0');
                    output_valid <= '0';
            end case;
        end if;
    end process;
    
    -- Saída do resultado
    output_result <= result;

end Behavioral;
```

Este código VHDL cria um processador simples que executa operações de soma, subtração e multiplicação entre dois números de 8 bits. O processador é implementado em uma arquitetura de pipeline de três estágios: busca de instrução, decodificação e execução.

Na etapa de busca de instrução, o processador incrementa o contador de programa (PC) a cada ciclo de clock para buscar a próxima instrução na memória. Quando o PC atinge o valor máximo de 255, ele é reiniciado para zero.

Na etapa de decodificação, a instrução é decodificada a partir dos sinais de entrada: o primeiro byte é o operando A, o segundo byte é o operando B e os três bits finais são o código de operação. Os operandos A e B são passados para a etapa de execução para realizar a operação especificada pelo código de operação.

Na etapa de execução, a operação é executada de acordo com o código de operação. Se o código de operação for "0000", a operação é uma soma; se for "0001", é uma subtração; e se for "0010", é uma multiplicação. O resultado é armazenado em um registrador interno chamado "result" e a saída é marcada como válida através do sinal "output_valid".

O resultado final é então retornado através do sinal "output_result" para uso externo.