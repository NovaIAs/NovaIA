Claro! A seguir, vou fornecer um exemplo de código em VHDL que implementa um processador MIPS simplificado. 

O MIPS (Microprocessor without Interlocked Pipeline Stages) é uma arquitetura RISC (Reduced Instruction Set Computer) amplamente utilizada em sistemas embarcados. Neste código, vamos implementar um processador MIPS simplificado com suporte a instruções básicas como adição, subtração e desvio condicional. 

Aqui está o código em VHDL:

```vhdl
-- Definindo a entidade do processador MIPS
entity MIPS_Processor is
    Port (
        clock       : in  std_logic;
        reset       : in  std_logic;
        instruction : in  std_logic_vector(31 downto 0);
        input_1     : in  std_logic_vector(31 downto 0);
        input_2     : in  std_logic_vector(31 downto 0);
        output      : out std_logic_vector(31 downto 0)
    );
end MIPS_Processor;

-- Implementação da arquitetura do processador MIPS
architecture Behavioral of MIPS_Processor is
    -- Registradores internos
    signal pc        : unsigned(31 downto 0);
    signal opcode    : std_logic_vector(5 downto 0);
    signal rs        : std_logic_vector(4 downto 0);
    signal rt        : std_logic_vector(4 downto 0);
    signal rd        : std_logic_vector(4 downto 0);
    signal funct     : std_logic_vector(5 downto 0);
    signal imm       : std_logic_vector(15 downto 0);
    signal sign_ext  : std_logic_vector(31 downto 0);
    signal alu_result: std_logic_vector(31 downto 0);
    signal branch    : std_logic;
    
begin

    -- Decodificação da instrução
    opcode    <= instruction(31 downto 26);
    rs        <= instruction(25 downto 21);
    rt        <= instruction(20 downto 16);
    rd        <= instruction(15 downto 11);
    funct     <= instruction(5 downto 0);
    imm       <= instruction(15 downto 0);
    sign_ext  <= imm & "00";
    
    -- Unidade de Controle
    process (opcode, funct)
    begin
        if opcode = "000000" then
            -- Instrução do tipo R
            case funct is
                when "100000" =>
                    -- add
                    alu_result <= std_logic_vector(unsigned(input_1) + unsigned(input_2));
                when "100010" =>
                    -- sub
                    alu_result <= std_logic_vector(unsigned(input_1) - unsigned(input_2));
                when others =>
                    -- Função não suportada
                    alu_result <= (others => 'X');
            end case;
        elsif opcode = "000100" then
            -- Instrução de Branch
            if input_1 = input_2 then
                branch <= '1';
            else
                branch <= '0';
            end if;
        else
            -- Opcode não suportado
            alu_result <= (others => 'X');
        end if;
    end process;
    
    -- Atualização do PC
    process (clock, reset)
    begin
        if reset = '1' then
            pc <= (others => '0');
        elsif rising_edge(clock) then
            if branch = '1' then
                pc <= pc + sign_ext;
            else
                pc <= pc + 4;
            end if;
        end if;
    end process;
    
    -- Seleção do resultado
    process (alu_result, rd)
    begin
        if rd = "00000" then
            output <= alu_result;
        else
            output <= input_2;
        end if;
    end process;

end Behavioral;
```

Neste código, definimos a entidade `MIPS_Processor` com os sinais de entrada (clock, reset, instruction, input_1, input_2) e o sinal de saída (output). 

A arquitetura `Behavioral` implementa o processador MIPS simplificado. Primeiro, decodificamos a instrução com base no `opcode` e `funct`. Em seguida, usamos uma unidade de controle para determinar a operação a ser executada. 

Para instruções do tipo R (opcode = "000000"), realizamos a operação adequada (adição ou subtração) nos sinais `input_1` e `input_2`. Para instruções de Branch (opcode = "000100"), comparamos `input_1` e `input_2` e definimos o sinal `branch` para indicar se o desvio deve ocorrer. 

Atualizamos o valor do PC (Program Counter) com base no resultado da unidade de controle e, por fim, selecionamos o resultado correto (resultado da ALU ou `input_2`) para atribuir ao sinal de saída `output`.