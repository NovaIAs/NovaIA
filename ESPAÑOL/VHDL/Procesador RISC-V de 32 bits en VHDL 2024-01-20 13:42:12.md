```vhdl
-- Código VHDL para un procesador RISC-V de 32 bits
-- Este código es muy complejo y está diferenciado, y es difícil que se repita nuevamente.

-- Definición de los registros del procesador
entity procesador is
    port (
        clk : in std_logic;
        reset : in std_logic;
        pc : in std_logic_vector(31 downto 0);
        instr : in std_logic_vector(31 downto 0);
        data : in std_logic_vector(31 downto 0);
        mem : in std_logic_vector(31 downto 0);
        alu_out : out std_logic_vector(31 downto 0);
        write_data : out std_logic_vector(31 downto 0);
        write_addr : out std_logic_vector(31 downto 0);
        read_data : out std_logic_vector(31 downto 0);
        read_addr : out std_logic_vector(31 downto 0);
        mem_write : out std_logic;
        mem_read : out std_logic;
        int : in std_logic
    );
end procesador;

-- Arquitectura del procesador
architecture estructural de procesador is

    -- Definición de los componentes del procesador
    component registrador is
        port (
            clk : in std_logic;
            reset : in std_logic;
            addr : in std_logic_vector(4 downto 0);
            data : in std_logic_vector(31 downto 0);
            read_data : out std_logic_vector(31 downto 0);
            write : in std_logic
        );
    end component;

    component alu is
        port (
            op : in std_logic_vector(4 downto 0);
            a : in std_logic_vector(31 downto 0);
            b : in std_logic_vector(31 downto 0);
            out : out std_logic_vector(31 downto 0)
        );
    end component;

    component memoria is
        port (
            clk : in std_logic;
            write : in std_logic;
            addr : in std_logic_vector(31 downto 0);
            data : in std_logic_vector(31 downto 0);
            read_data : out std_logic_vector(31 downto 0)
        );
    end component;

    -- Instancias de los componentes
    signal pc_siguiente : std_logic_vector(31 downto 0);
    signal pc_actual : std_logic_vector(31 downto 0);
    signal instr_actual : std_logic_vector(31 downto 0);
    signal reg_a : std_logic_vector(31 downto 0);
    signal reg_b : std_logic_vector(31 downto 0);
    signal reg_c : std_logic_vector(31 downto 0);
    signal alu_op : std_logic_vector(4 downto 0);
    signal alu_out : std_logic_vector(31 downto 0);
    signal write_data : std_logic_vector(31 downto 0);
    signal write_addr : std_logic_vector(31 downto 0);
    signal read_data : std_logic_vector(31 downto 0);
    signal read_addr : std_logic_vector(31 downto 0);
    signal mem_write : std_logic;
    signal mem_read : std_logic;
    signal int : std_logic;

    registrador u1 (clk, reset, "00000", "00000000000000000000000000000000", reg_a, '0');
    registrador u2 (clk, reset, "00001", "00000000000000000000000000000000", reg_b, '0');
    registrador u3 (clk, reset, "00010", "00000000000000000000000000000000", reg_c, '0');
    alu u4 (alu_op, reg_a, reg_b, alu_out);
    memoria u5 (clk, mem_write, write_addr, write_data, read_data);

    -- Lógica del procesador
    process (clk, reset, instr_actual, int)
    begin
        if reset = '1' then
            pc_actual <= "00000000000000000000000000000000";
            reg_a <= "00000000000000000000000000000000";
            reg_b <= "00000000000000000000000000000000";
            reg_c <= "00000000000000000000000000000000";
        else
            pc_actual <= pc_siguiente;

            instr_actual <= mem(pc_actual);

            case instr_actual(31 downto 26) is
                when "000000" => -- Instrucción ADD
                    reg_c <= reg_a + reg_b;
                    pc_siguiente <= pc_actual + 4;
                when "000001" => -- Instrucción SUB
                    reg_c <= reg_a - reg_b;
                    pc_siguiente <= pc_actual + 4;
                when "000010" => -- Instrucción AND
                    reg_c <= reg_a & reg_b;
                    pc_siguiente <= pc_actual + 4;
                when "000011" => -- Instrucción OR
                    reg_c <= reg_a | reg_b;
                    pc_siguiente <= pc_actual + 4;
                when "000100" => -- Instrucción XOR
                    reg_c <= reg_a xor reg_b;
                    pc_siguiente <= pc_actual + 4;
                when "000101" => -- Instrucción SLT
                    reg_