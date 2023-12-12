Claro! Aqui está um exemplo de um código VHDL complexo e diferenciado. Este código implementa um processador RISC de 8 bits com instruções básicas de carga, armazenamento, soma, subtração, desvio condicional e desvio incondicional.

```vhdl
-- Definição das constantes
constant WORD_SIZE : integer := 8;
constant MEMORY_SIZE : integer := 256;

-- Definição do tipo de dados para o processador
type Processor is record
    PC : integer range 0 to MEMORY_SIZE;
    IR : std_logic_vector(WORD_SIZE-1 downto 0);
    A : std_logic_vector(WORD_SIZE-1 downto 0);
    B : std_logic_vector(WORD_SIZE-1 downto 0);
    ALU_output : std_logic_vector(WORD_SIZE-1 downto 0);
    memory : array(0 to MEMORY_SIZE-1) of std_logic_vector(WORD_SIZE-1 downto 0);
end record;

-- Componentes internos do processador
component ALU is
    port(
        op1 : in std_logic_vector(WORD_SIZE-1 downto 0);
        op2 : in std_logic_vector(WORD_SIZE-1 downto 0);
        opcode : in std_logic_vector(2 downto 0);
        result : out std_logic_vector(WORD_SIZE-1 downto 0)
    );
end component;

-- Componente da ALU (Unidade Lógica Aritmética)
component ALU is
    port(
        op1 : in std_logic_vector(WORD_SIZE-1 downto 0);
        op2 : in std_logic_vector(WORD_SIZE-1 downto 0);
        opcode : in std_logic_vector(2 downto 0);
        result : out std_logic_vector(WORD_SIZE-1 downto 0)
    );
end component;

-- Componente da memória
component Memory is
    port(
        address : in integer range 0 to MEMORY_SIZE-1;
        data_in : in std_logic_vector(WORD_SIZE-1 downto 0);
        data_out : out std_logic_vector(WORD_SIZE-1 downto 0)
    );
end component;

-- Componente principal do processador
entity Processor is
    port(
        clk : in std_logic;
        reset : in std_logic;
        enable : in std_logic;
        data_out : out std_logic_vector(WORD_SIZE-1 downto 0)
    );
end entity;

architecture Behavioral of Processor is
    constant OPCODE_WIDTH : integer := 3;
    signal opcode : std_logic_vector(OPCODE_WIDTH-1 downto 0);
    signal next_PC : integer range 0 to MEMORY_SIZE;
    signal next_A : std_logic_vector(WORD_SIZE-1 downto 0);
    signal next_B : std_logic_vector(WORD_SIZE-1 downto 0);
    signal next_ALU_output : std_logic_vector(WORD_SIZE-1 downto 0);
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                PC <= 0;
                A <= (others => '0');
                B <= (others => '0');
            elsif enable = '1' then
                PC <= next_PC;
                A <= next_A;
                B <= next_B;
                ALU_output <= next_ALU_output;
            end if;
        end if;
    end process;
    
    process(PC, memory, IR, ALU_output)
    begin
        opcode <= IR(IR'high downto IR'high-OPCODE_WIDTH+1);
        
        case opcode is
            when "000" => -- Carga (load)
                next_A <= memory(to_integer(unsigned(IR(IR'high-1 downto IR'high-5))));
                next_B <= B;
                next_ALU_output <= next_A;
                next_PC <= PC + 1;
                
            when "001" => -- Armazenamento (store)
                next_A <= A;
                next_B <= B;
                next_ALU_output <= B;
                next_PC <= PC + 1;
                
            when "010" => -- Soma
                next_A <= A;
                next_B <= B;
                next_ALU_output <= ALU_output + B;
                next_PC <= PC + 1;
                
            when "011" => -- Subtração
                next_A <= A;
                next_B <= B;
                next_ALU_output <= ALU_output - B;
                next_PC <= PC + 1;
                
            when "100" => -- Desvio condicional (avança se zero)
                next_A <= A;
                next_B <= B;
                next_ALU_output <= ALU_output;
                if ALU_output = (others => '0') then
                    next_PC <= PC + 1;
                else
                    next_PC <= PC + to_integer(unsigned(IR(IR'high-1 downto IR'high-5)));
                end if;
                
            when "101" => -- Desvio incondicional
                next_A <= A;
                next_B <= B;
                next_ALU_output <= ALU_output;
                next_PC <= PC + to_integer(unsigned(IR(IR'high-1 downto IR'high-5)));
                
            when others => -- Instrução inválida
                next_A <= A;
                next_B <= B;
                next_ALU_output <= ALU_output;
                next_PC <= PC + 1;
        end case;
    end process;
    
    ALU_inst : ALU
        port map(
            op1 => A,
            op2 => B,
            opcode => opcode,
            result => ALU_output
        );
    
    Memory_inst : Memory
        port map(
            address => PC,
            data_in => IR,
            data_out => data_out
        );
end architecture;
```

Este código é apenas um exemplo e pode ser adaptado de acordo com suas necessidades. Ele implementa as operações básicas de um processador RISC de 8 bits, como carga, armazenamento, soma, subtração e desvios condicionais e incondicionais. É importante ressaltar que este código é uma representação simplificada e algumas funcionalidades podem não estar completamente implementadas.

Espero que este código seja útil para você! Se tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.