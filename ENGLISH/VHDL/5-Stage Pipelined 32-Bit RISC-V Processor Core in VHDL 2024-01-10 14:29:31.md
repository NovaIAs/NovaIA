```vhdl
-- This VHDL code implements a 32-bit RISC-V processor core with a 5-stage pipeline.

-- Define the instruction set architecture (ISA) for the processor.
library ieee;
use ieee.std_logic_1164.all;

-- Define the register file for the processor.
entity register_file is
    port (
        clk : in std_logic;
        rst : in std_logic;

        -- Read ports
        read_address_1 : in std_logic_vector(4 downto 0);
        read_address_2 : in std_logic_vector(4 downto 0);

        -- Write port
        write_address : in std_logic_vector(4 downto 0);
        write_data : in std_logic_vector(31 downto 0);
        write_enable : in std_logic
    );
end entity;

architecture behavioral of register_file is
    type register_array is array (0 to 31) of std_logic_vector(31 downto 0);

    signal registers : register_array := (others => (others => '0'));

begin
    -- Read ports
    read_data_1 <= registers(read_address_1);
    read_data_2 <= registers(read_address_2);

    -- Write port
    process (clk)
    begin
        if rising_edge(clk) and rst = '0' then
            if write_enable = '1' then
                registers(write_address) <= write_data;
            end if;
        end if;
    end process;
end architecture;

-- Define the arithmetic logic unit (ALU) for the processor.
entity alu is
    port (
        op : in std_logic_vector(2 downto 0);
        a : in std_logic_vector(31 downto 0);
        b : in std_logic_vector(31 downto 0);

        result : out std_logic_vector(31 downto 0);
        zero : out std_logic
    );
end entity;

architecture behavioral of alu is
begin
    process (op, a, b)
    begin
        case op is
            when "000" => result <= a + b;
            when "001" => result <= a - b;
            when "010" => result <= a & b;
            when "011" => result <= a | b;
            when "100" => result <= a ^ b;
            when "101" => result <= ~a;
            when "110" => result <= a << b(4 downto 0);
            when "111" => result <= a >> b(4 downto 0);
            when others => result <= (others => '0');
        end case;

        zero <= result = (others => '0');
    end process;
end architecture;

-- Define the control unit for the processor.
entity control_unit is
    port (
        clk : in std_logic;
        rst : in std_logic;

        instruction : in std_logic_vector(31 downto 0);

        -- Control signals
        reg_write : out std_logic;
        alu_op : out std_logic_vector(2 downto 0);
        branch : out std_logic;
        jump : out std_logic;
        mem_write : out std_logic;
        mem_read : out std_logic;
    );
end entity;

architecture behavioral of control_unit is
begin
    process (clk)
    begin
        if rising_edge(clk) and rst = '0' then
            case instruction(6 downto 0) is
                when "0110011" => -- ADD
                    reg_write <= '1';
                    alu_op <= "000";
                    branch <= '0';
                    jump <= '0';
                    mem_write <= '0';
                    mem_read <= '0';
                when "0010011" => -- SUB
                    reg_write <= '1';
                    alu_op <= "001";
                    branch <= '0';
                    jump <= '0';
                    mem_write <= '0';
                    mem_read <= '0';
                when "0000011" => -- AND
                    reg_write <= '1';
                    alu_op <= "010";
                    branch <= '0';
                    jump <= '0';
                    mem_write <= '0';
                    mem_read <= '0';
                when "0001011" => -- OR
                    reg_write <= '1';
                    alu_op <= "011";
                    branch <= '0';
                    jump <= '0';
                    mem_write <= '0';
                    mem_read <= '0';
                when "0011011" => -- XOR
                    reg_write <= '1';
                    alu_op <= "100";
                    branch <= '0';
                    jump <= '0';
                    mem_write <= '0';
                    mem_read <= '0';
                when "1100011" => -- NOT
                    reg_write <= '1';
                    alu_op <= "101";
                    branch <= '0';
                    jump <= '0';
                    mem_write <= '0';
                    mem_read <= '0';
                when "0000111" => -- SL