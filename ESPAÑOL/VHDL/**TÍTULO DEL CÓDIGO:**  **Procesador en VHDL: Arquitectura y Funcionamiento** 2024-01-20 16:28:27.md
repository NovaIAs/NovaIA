```vhdl
-- **MÓDULO PRINCIPAL DEL PROCESADOR**
ENTIDAD procesador ES
    PUERTO (
        -- Reloj y señal de reinicio
        clk               : IN STD_LOGIC;
        rst               : IN STD_LOGIC;

        -- Entradas de memoria de programa
        instr_in          : IN STD_LOGIC_VECTOR (31 DOWNTO 0);

        -- Salidas de memoria de datos
        data_out          : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);

        -- Direcciones de memoria
        addr_out          : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);

        -- Salidas de ALU
        alu_result        : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);

        -- Señales de control
        reg_write         : OUT STD_LOGIC;
        mem_write         : OUT STD_LOGIC;
        mem_read          : OUT STD_LOGIC;
        jump              : OUT STD_LOGIC;
        branch            : OUT STD_LOGIC;
        mux_sel           : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);

        -- Salidas de registro
        reg_a_out         : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
        reg_b_out         : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);

        -- Entradas de registro
        reg_a_in          : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
        reg_b_in          : IN STD_LOGIC_VECTOR (31 DOWNTO 0)
    );
END ENTIDAD procesador;

-- **ARQUITECTURA DEL PROCESADOR**
ARQUITECTURA proc_arch DE procesador ES

-- **REGISTROS**
-- Registro de programa contador (PC)
SEÑAL pc                     : STD_LOGIC_VECTOR (31 DOWNTO 0);

-- Registro de instrucción (IR)
SEÑAL ir                     : STD_LOGIC_VECTOR (31 DOWNTO 0);

-- Registro de dirección de memoria de datos (MDR)
SEÑAL mdr                    : STD_LOGIC_VECTOR (31 DOWNTO 0);

-- Registro de datos de memoria de datos (MDR)
SEÑAL data_reg               : STD_LOGIC_VECTOR (31 DOWNTO 0);

-- Registro A
SEÑAL reg_a                   : STD_LOGIC_VECTOR (31 DOWNTO 0);

-- Registro B
SEÑAL reg_b                   : STD_LOGIC_VECTOR (31 DOWNTO 0);

-- **SEÑALES DE CONTROL**
-- Señal de escritura de registro
SEÑAL reg_write_sig          : STD_LOGIC;

-- Señal de escritura de memoria de datos
SEÑAL mem_write_sig          : STD_LOGIC;

-- Señal de lectura de memoria de datos
SEÑAL mem_read_sig           : STD_LOGIC;

-- Señal de salto
SEÑAL jump_sig              : STD_LOGIC;

-- Señal de rama
SEÑAL branch_sig            : STD_LOGIC;

-- Señal de selección de multiplexor
SEÑAL mux_sel_sig           : STD_LOGIC_VECTOR (1 DOWNTO 0);

-- **COMPONENTES**
-- Unidad de control
COMPONENTE control ES
    PUERTO (
        -- Reloj y señal de reinicio
        clk               : IN STD_LOGIC;
        rst               : IN STD_LOGIC;

        -- Salidas de control
        reg_write         : OUT STD_LOGIC;
        mem_write         : OUT STD_LOGIC;
        mem_read          : OUT STD_LOGIC;
        jump              : OUT STD_LOGIC;
        branch            : OUT STD_LOGIC;
        mux_sel           : OUT STD_LOGIC_VECTOR (1 DOWNTO 0)
    );
END COMPONENTE;

-- Unidad aritmético-lógica (ALU)
COMPONENTE alu ES
    PUERTO (
        -- Entradas de datos
        data_a            : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
        data_b            : IN STD_LOGIC_VECTOR (31 DOWNTO 0);

        -- Señal de selección de operación
        op                : IN STD_LOGIC_VECTOR (3 DOWNTO 0);

        -- Salida de resultado
        result             : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
    );
END COMPONENTE;

-- Registro
COMPONENTE register ES
    PUERTO (
        -- Reloj y señal de reinicio
        clk               : IN STD_LOGIC;
        rst               : IN STD_LOGIC;

        -- Señal de escritura
        write             : IN STD_LOGIC;

        -- Entrada de datos
        data_in           : IN STD_LOGIC_VECTOR (31 DOWNTO 0);

        -- Salida de datos
        data_out          : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
    );
END COMPONENTE;

-- **INSTANCIACIÓN DE COMPONENTES**
-- Unidad de control
u_control : control
    PORT MAP (
        clk               => clk,
        rst               => rst,
        reg_write         => reg_write_sig,
        mem_write         => mem_write_sig,
        mem_read          => mem_read_sig,
        jump              => jump_sig,
        branch            => branch_sig,
        mux_sel           => mux_sel_sig
    );

-- Unidad aritmético-lógica (ALU)
u_alu : alu
    PORT MAP (
        data_a            => reg_a,
        data_b            => reg_b,
        op                => ir(31 DOWNTO 28),
        result             => alu_result
    );

-- Registro de programa contador (PC)
u_pc : register
    PORT MAP (
        clk               => clk,
        rst               => rst,
        write             => jump_sig OR branch_sig,
        data_in           => pc + 4,
        data_out          => pc
    );

-- Registro de instrucción (IR)
u_ir : register
    PORT MAP (
        clk               => clk,
        rst               => rst,
        write             => 1,
        data_in           => instr_in,
        data_out          => ir
    );

-- Registro de dirección de memoria de datos (MDR)
u_mdr : register
    PORT MAP (
        clk               => clk,
        rst               => rst,
        write             => 1,
        data_in           => alu_result,
        data_out          => mdr
    );

-- Registro de datos de memoria de datos (MDR)
u_data_reg : register
    PORT MAP (
        clk               => clk,
        rst               => rst,
        write             => mem_write_sig,
        data_in           => data_in,
        data_out          => data_reg
    );

-- Registro A
u_reg_a : register
    PORT MAP (
        clk               => clk,
        rst               => rst,
        write             => reg_write_sig,
        data_in           => reg_b_out,
        data_out          => reg_a
    );

-- Registro B
u_reg_b : register
    PORT MAP (
        clk               => clk,
        rst               => rst,
        write             => reg_write_sig,
        data_in           => mux_sel_sig(1) ? data_reg : alu_result,
        data_out          => reg_b
    );

-- **ASIGNACIÓN DE SEÑALES**
-- Salidas de memoria de datos
data_out <= data_reg;

-- Direcciones de memoria
addr_out <= mdr;

-- Señales de control
reg_write <= reg_write_sig;
mem_write <= mem_write_sig;
mem_read <= mem_read_sig;
jump <= jump_sig;
branch <= branch_sig;
mux_sel <= mux_sel_sig;

-- Salidas de registro
reg_a_out <= reg_a;
reg_b_out <= reg_b;

END ARQUITECTURA proc_arch;
```

**Explicación del código:**

1. **Módulo principal del procesador (processor):** Define la interfaz del procesador, especificando las entradas y salidas del módulo.

2. **Arquitectura del procesador (proc_arch):** Implementa la arquitectura del procesador.

3. **Registro de programa contador (PC):** Mantiene la dirección de la siguiente instrucción a ejecutar.

4. **Registro de instrucción (IR):** Almacena la instrucción actual que se está ejecutando.

5. **Registro de dirección de memoria de datos (MDR):** Almacena la dirección de la memoria de datos a la que se accede.

6. **Registro de datos de memoria de datos (MDR):** Almacena los datos que se leen o escriben en la memoria de datos.

7. **Registro A:** Registr