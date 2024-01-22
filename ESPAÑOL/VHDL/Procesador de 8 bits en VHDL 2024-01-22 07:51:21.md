```vhdl
-- Esta es una entidad en VHDL para un procesador de 8 bits.
entity procesador8bits is
    -- Estas son las entradas del procesador.
    port (
        clk : in std_logic;
        rst : in std_logic;
        inst : in std_logic_vector(15 downto 0);
        data : in std_logic_vector(7 downto 0);
    );

    -- Estas son las salidas del procesador.
    port (
        pc : out std_logic_vector(15 downto 0);
        reg : out std_logic_vector(7 downto 0);
        mem : out std_logic_vector(7 downto 0);
    );
end entity procesador8bits;

-- Esta es la arquitectura del procesador.
architecture estructural de procesador8bits is

    -- Este es el registro del programador.
    signal pc : std_logic_vector(15 downto 0);

    -- Estos son los registros de la memoria.
    signal mem : std_logic_vector(7 downto 0);

    -- Este es el registro del acumulador.
    signal reg : std_logic_vector(7 downto 0);

    -- Este es el registro de instrucción.
    signal inst : std_logic_vector(15 downto 0);

    -- Este es el decodificador de instrucciones.
    decodificador_de_instrucciones : process (inst) is
    begin
        case inst is
            when "0000000000000000" => -- ADD
                reg <= reg + data;
            when "0000000000000001" => -- SUB
                reg <= reg - data;
            when "0000000000000010" => -- AND
                reg <= reg and data;
            when "0000000000000011" => -- OR
                reg <= reg or data;
            when "0000000000000100" => -- XOR
                reg <= reg xor data;
            when "0000000000000101" => -- NOT
                reg <= not reg;
            when "0000000000000110" => -- MOV
                reg <= data;
            when "0000000000000111" => -- LDI
                reg <= inst(7 downto 0);
            when "0000000000001000" => -- STA
                mem <= reg;
            when "0000000000001001" => -- LDA
                reg <= mem;
            when "0000000000001010" => -- BEQ
                if reg = data then
                    pc <= pc + 1;
                end if;
            when "0000000000001011" => -- BNE
                if reg /= data then
                    pc <= pc + 1;
                end if;
            when "0000000000001100" => -- BLT
                if reg < data then
                    pc <= pc + 1;
                end if;
            when "0000000000001101" => -- BGT
                if reg > data then
                    pc <= pc + 1;
                end if;
            when "0000000000001110" => -- BLE
                if reg <= data then
                    pc <= pc + 1;
                end if;
            when "0000000000001111" => -- BGE
                if reg >= data then
                    pc <= pc + 1;
                end if;
            when others =>
                -- No hay ninguna instrucción válida.
        end case;
    end process decodificador_de_instrucciones;

    -- Este es el contador de programa.
    contador_de_programa : process (clk, rst) is
    begin
        if rst = '1' then
            pc <= (others => '0');
        elsif clk'event and clk = '1' then
            pc <= pc + 1;
        end if;
    end process contador_de_programa;

    -- Esta es la memoria.
    memoria : process (clk, rst, pc, data) is
    begin
        if rst = '1' then
            mem <= (others => '0');
        elsif clk'event and clk = '1' then
            if inst(15 downto 8) = "00000000"