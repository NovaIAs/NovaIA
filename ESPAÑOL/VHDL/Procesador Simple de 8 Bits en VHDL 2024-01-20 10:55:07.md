```vhdl

-- Módulo superior de un procesador simple de 8 bits

-- Definición de la arquitectura del procesador
architecture Processor_Arch of Processor is

    -- Definición de las señales internas
    signal PC : std_logic_vector(7 downto 0);
    signal IR : std_logic_vector(7 downto 0);
    signal A : std_logic_vector(7 downto 0);
    signal B : std_logic_vector(7 downto 0);
    signal ALU_Result : std_logic_vector(7 downto 0);
    signal Status : std_logic_vector(3 downto 0);

    -- Definición de los componentes internos
    component Memory is
        port(
            Address : in std_logic_vector(7 downto 0);
            Data_In : in std_logic_vector(7 downto 0);
            Data_Out : out std_logic_vector(7 downto 0)
        );
    end component;

    component ALU is
        port(
            A : in std_logic_vector(7 downto 0);
            B : in std_logic_vector(7 downto 0);
            Op : in std_logic_vector(2 downto 0);
            Result : out std_logic_vector(7 downto 0);
            Status : out std_logic_vector(3 downto 0)
        );
    end component;

    component Register_File is
        port(
            Address_A : in std_logic_vector(2 downto 0);
            Address_B : in std_logic_vector(2 downto 0);
            Data_In : in std_logic_vector(7 downto 0);
            Data_A : out std_logic_vector(7 downto 0);
            Data_B : out std_logic_vector(7 downto 0)
        );
    end component;

begin

    -- Instancias de los componentes internos
    Memory_Inst : Memory port map(
        Address => PC,
        Data_In => B,
        Data_Out => IR
    );

    ALU_Inst : ALU port map(
        A => A,
        B => B,
        Op => IR(7 downto 5),
        Result => ALU_Result,
        Status => Status
    );

    Register_File_Inst : Register_File port map(
        Address_A => IR(4 downto 2),
        Address_B => IR(2 downto 0),
        Data_In => ALU_Result,
        Data_A => A,
        Data_B => B
    );

    -- Lógica de control del procesador
    process(IR) is
    begin
        case IR(7 downto 5) is
            when "000" => -- ADD
                A <= A + B;
            when "001" => -- SUB
                A <= A - B;
            when "010" => -- AND
                A <= A & B;
            when "011" => -- OR
                A <= A | B;
            when "100" => -- XOR
                A <= A ^ B;
            when "101" => -- LD
                A <= IR(2 downto 0);
            when "110" => -- ST
                B <= A;
            when "111" => -- JMP
                PC <= IR(2 downto 0);
        end case;

        -- Actualización del contador de programa
        PC <= PC + 1;
    end process;

end Processor_Arch;

```

**Explicación:**

Este código VHDL es el código fuente de un procesador simple de 8 bits. El procesador está compuesto por los siguientes componentes:

* **Memoria:** La memoria almacena las instrucciones y los datos del programa.
* **Unidad Aritmética y Lógica (ALU):** La ALU realiza operaciones aritméticas y lógicas en los datos.
* **Archivo de Registros:** El archivo de registros almacena los datos de los registros del procesador.
* **Contador de Programa (PC):** El PC indica la dirección de la siguiente instrucción que se debe ejecutar.
* **Registro de Instrucciones (IR):** El IR almacena la instrucción que se está ejecutando actualmente.
* **Registro A:** El registro A es uno de los registros de propósito general del procesador.
* **Registro B:** El registro B es otro de los registros de propósito general del procesador.
* **Registro de Estado:** El registro de estado almacena el estado del procesador, como los indicadores de condición.

El código VHDL está organizado en tres secciones:

* **Declaración de la arquitectura:** Esta sección declara la arquitectura del procesador, que incluye las señales internas, los componentes internos y la lógica de control del procesador.
* **Definición de los componentes internos:** Esta sección define los componentes internos del procesador, que incluyen la memoria, la ALU, el archivo de registros y el contador de programa.
* **Cuerpo de la arquitectura:** Esta sección contiene la lógica de control del procesador, que incluye la lógica para decodificar las instrucciones y para actualizar el contador de programa.

El código VHDL es complejo y difícil de entender, pero es un ejemplo muy poderoso de cómo se puede utilizar el lenguaje VHDL para diseñar circuitos digitales.