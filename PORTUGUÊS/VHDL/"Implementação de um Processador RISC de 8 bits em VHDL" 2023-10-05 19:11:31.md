Claro! Aqui está um exemplo de código complexo em VHDL, que implementa um processador RISC de 8 bits. O código a seguir é dividido em diferentes entidades e arquivos, para facilitar a compreensão e a organização do projeto.

Entity: CPU

```vhdl
entity CPU is
    port (
        clk     : in std_logic;
        reset   : in std_logic;
        address : in std_logic_vector(7 downto 0);
        data_in : in std_logic_vector(7 downto 0);
        data_out: out std_logic_vector(7 downto 0)
    );
end CPU;
```

Architecture: CPU

```vhdl
architecture Behavioral of CPU is
    type reg_array is array (0 to 15) of std_logic_vector(7 downto 0);
    signal reg_file : reg_array;

    signal pc : std_logic_vector(7 downto 0);
    signal ir : std_logic_vector(7 downto 0);
    
    component ALU is
        port (
            a, b     : in std_logic_vector(7 downto 0);
            opcode   : in std_logic_vector(2 downto 0);
            result   : out std_logic_vector(7 downto 0);
            zero     : out std_logic;
            overflow : out std_logic
        );
    end component;
    
    component ControlUnit is
        port (
            opcode   : in std_logic_vector(2 downto 0);
            enable   : in std_logic;
            clk      : in std_logic;
            reset    : in std_logic;
            reg_write: out std_logic;
            alu_op   : out std_logic_vector(2 downto 0)
        );
    end component;

    component Memory is
        port (
            clk     : in std_logic;
            address : in std_logic_vector(7 downto 0);
            data_in : in std_logic_vector(7 downto 0);
            read    : in std_logic;
            write   : in std_logic;
            data_out: out std_logic_vector(7 downto 0)
        );
    end component;
    
    component Register is
        port (
            clk     : in std_logic;
            reset   : in std_logic;
            data_in : in std_logic_vector(7 downto 0);
            enable  : in std_logic;
            data_out: out std_logic_vector(7 downto 0)
        );
    end component;
    
begin
    -- Unidade de Controle
    ControlUnit_inst: ControlUnit port map (
        opcode    => ir(7 downto 5),
        enable    => not reset,
        clk       => clk,
        reset     => reset,
        reg_write => reg_write,
        alu_op    => alu_op
    );
    
    -- Memória
    Memory_inst: Memory port map (
        clk      => clk,
        address  => address,
        data_in  => data_in,
        read     => not reset,
        write    => reg_write,
        data_out => ir
    );

    -- Registrador PC
    RegisterPC_inst: Register port map (
        clk      => clk,
        reset    => reset,
        data_in  => pc,
        enable   => not reset,
        data_out => pc
    );
    
    -- Registradores Gerais
    RegisterFile_inst: for i in 0 to 15 generate
        Register_inst: Register port map (
            clk      => clk,
            reset    => reset,
            data_in  => reg_file(i),
            enable   => not reset,
            data_out => reg_file(i)
        );
    end generate;
    
    -- Unidade Aritmética e Lógica (ALU)
    ALU_inst: ALU port map (
        a        => reg_file(to_integer(unsigned(ir(4 downto 3)))),
        b        => reg_file(to_integer(unsigned(ir(2 downto 0)))),
        opcode   => alu_op,
        result   => reg_file(to_integer(unsigned(ir(7 downto 5)))),
        zero     => open,
        overflow => open
    );
    
    -- Saída de Dados
    data_out <= reg_file(to_integer(unsigned(ir(7 downto 5))));
    
end Behavioral;
```

Entity: ALU

```vhdl
entity ALU is
    port (
        a, b     : in std_logic_vector(7 downto 0);
        opcode   : in std_logic_vector(2 downto 0);
        result   : out std_logic_vector(7 downto 0);
        zero     : out std_logic;
        overflow : out std_logic
    );
end ALU;
```

Architecture: ALU

```vhdl
architecture Behavioral of ALU is
begin
    process(a, b, opcode)
        variable temp_result : std_logic_vector(7 downto 0);
    begin
        temp_result := (others => '0');
        
        case opcode is
            when "000" => -- ADD
                temp_result := std_logic_vector(unsigned(a) + unsigned(b));
            
            when "001" => -- SUB
                temp_result := std_logic_vector(unsigned(a) - unsigned(b));
            
            when "010" => -- AND
                temp_result := a and b;
            
            when "011" => -- OR
                temp_result := a or b;
            
            when "100" => -- XOR
                temp_result := a xor b;
            
            when "101" => -- SLT
                if signed(a) < signed(b) then
                    temp_result := x"01";
                else
                    temp_result := x"00";
                end if;
            
            when "110" => -- SLL
                temp_result := shift_left(a, to_integer(unsigned(b(3 downto 0))));
            
            when "111" => -- SRL
                temp_result := shift_right(a, to_integer(unsigned(b(3 downto 0))));
            
            when others =>
                temp_result := (others => 'X');
        end case;
        
        result <= temp_result;
        
        zero <= '1' when temp_result = (others => '0') else '0';
        overflow <= '0';
    end process;
end Behavioral;
```

Entity: ControlUnit

```vhdl
entity ControlUnit is
    port (
        opcode   : in std_logic_vector(2 downto 0);
        enable   : in std_logic;
        clk      : in std_logic;
        reset    : in std_logic;
        reg_write: out std_logic;
        alu_op   : out std_logic_vector(2 downto 0)
    );
end ControlUnit;
```

Architecture: ControlUnit

```vhdl
architecture Behavioral of ControlUnit is
begin
    process(opcode, enable, clk, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                reg_write <= '0';
                alu_op <= (others => '0');
            elsif enable = '1' then
                case opcode is
                    when "000" => -- ADD
                        reg_write <= '1';
                        alu_op <= "000";
                    
                    when "001" => -- SUB
                        reg_write <= '1';
                        alu_op <= "001";
                    
                    when "010" => -- AND
                        reg_write <= '1';
                        alu_op <= "010";
                    
                    when "011" => -- OR
                        reg_write <= '1';
                        alu_op <= "011";
                    
                    when "100" => -- XOR
                        reg_write <= '1';
                        alu_op <= "100";
                    
                    when "101" => -- SLT
                        reg_write <= '1';
                        alu_op <= "101";
                    
                    when "110" => -- SLL
                        reg_write <= '1';
                        alu_op <= "110";
                    
                    when "111" => -- SRL
                        reg_write <= '1';
                        alu_op <= "111";
                    
                    when others =>
                        reg_write <= '0';
                        alu_op <= (others => '0');
                end case;
            end if;
        end if;
    end process;
end Behavioral;
```

Entity: Memory

```vhdl
entity Memory is
    port (
        clk     : in std_logic;
        address : in std_logic_vector(7 downto 0);
        data_in : in std_logic_vector(7 downto 0);
        read    : in std_logic;
        write   : in std_logic;
        data_out: out std_logic_vector(7 downto 0)
    );
end Memory;
```

Architecture: Memory

```vhdl
architecture Behavioral of Memory is
    type mem_array is array (0 to 255) of std_logic_vector(7 downto 0);
    signal memory : mem_array;
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if read = '1' then
                data_out <= memory(to_integer(unsigned(address)));
            elsif write = '1' then
                memory(to_integer(unsigned(address))) <= data_in;
            end if;
        end if;
    end process;
end Behavioral;
```

Entity: Register

```vhdl
entity Register is
    port (
        clk     : in std_logic;
        reset   : in std_logic;
        data_in : in std_logic_vector(7 downto 0);
        enable  : in std_logic;
        data_out: out std_logic_vector(7 downto 0)
    );
end Register;
```

Architecture: Register

```vhdl
architecture Behavioral of Register is
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                data_out <= (others => '0');
            elsif enable = '1' then
                data_out <= data_in;
            end if;
        end if;
    end process;
end Behavioral;
```

Este código representa um processador RISC simples de 8 bits, com memória, registradores, unidade aritmética e lógica (ALU) e uma unidade de controle que determina as operações a serem executadas pela ALU e o fluxo de dados entre as diferentes partes do processador.

Espero que este código lhe seja útil! Se tiver mais alguma dúvida, sinta-se à vontade para perguntar.