```vhdl
-- Código VHDL para un procesador de 8 bits con instrucciones aritméticas y lógicas

-- Definición de los pines de entrada y salida del procesador

entrada pin clk: std_logic;   -- Reloj del procesador
entrada pin reset: std_logic;  -- Señal de reinicio del procesador

entrada pin [7:0] datos: std_logic_vector(7 downto 0);  -- Datos de entrada al procesador
salida pin [7:0] resultado: std_logic_vector(7 downto 0); -- Resultado del procesamiento

-- Definición de los registros internos del procesador

tipo registro is record
    pc: std_logic_vector(7 downto 0);  -- Contador de programa
    ac: std_logic_vector(7 downto 0);   -- Acumulador
    ir: std_logic_vector(7 downto 0);   -- Registro de instrucción
    mar: std_logic_vector(7 downto 0);  -- Registro de dirección de memoria
    mdr: std_logic_vector(7 downto 0);  -- Registro de datos de memoria
    flags: std_logic_vector(2 downto 0); -- Registro de banderas
end record;

señal registro: registro; -- Registro interno del procesador

-- Definición de las instrucciones del procesador

tipo instrucción is (
    add,    -- Suma
    sub,    -- Resta
    and,    -- Y lógico
    or,     -- O lógico
    not,    -- Negación
    ld,     -- Carga
    st,     -- Almacenamiento
    jmp,    -- Salto
    je,     -- Salto si es igual
    jne,    -- Salto si es diferente
    jl,     -- Salto si es menor que
    jle,    -- Salto si es menor o igual que
    jg,     -- Salto si es mayor que
    jge,    -- Salto si es mayor o igual que
    call,   -- Llamada a subrutina
    ret     -- Retorno de subrutina
);

-- Definición del código de operación de las instrucciones

constante opcode_add: std_logic_vector(7 downto 0) := "00000001";
constante opcode_sub: std_logic_vector(7 downto 0) := "00000010";
constante opcode_and: std_logic_vector(7 downto 0) := "00000011";
constante opcode_or: std_logic_vector(7 downto 0) := "00000100";
constante opcode_not: std_logic_vector(7 downto 0) := "00000101";
constante opcode_ld: std_logic_vector(7 downto 0) := "00000110";
constante opcode_st: std_logic_vector(7 downto 0) := "00000111";
constante opcode_jmp: std_logic_vector(7 downto 0) := "00001000";
constante opcode_je: std_logic_vector(7 downto 0) := "00001001";
constante opcode_jne: std_logic_vector(7 downto 0) := "00001010";
constante opcode_jl: std_logic_vector(7 downto 0) := "00001011";
constante opcode_jle: std_logic_vector(7 downto 0) := "00001100";
constante opcode_jg: std_logic_vector(7 downto 0) := "00001101";
constante opcode_jge: std_logic_vector(7 downto 0) := "00001110";
constante opcode_call: std_logic_vector(7 downto 0) := "00001111";
constante opcode_ret: std_logic_vector(7 downto 0) := "00010000";

-- Definición de la memoria del procesador

tipo memoria is array (std_logic_vector(7 downto 0)) of std_logic_vector(7 downto 0);

señal memoria: memoria;

-- Definición del ciclo de reloj del procesador

proceso (clk)
    begin
        -- En el borde positivo del reloj, se ejecuta el ciclo de instrucción del procesador

        if rising_edge(clk) then

            -- Restablecimiento del procesador

            if reset = '1' then
                registro.pc <= "00000000";
                registro.ac <= "00000000";
                registro.ir <= "00000000";
                registro.mar <= "00000000";
                registro.mdr <= "00000000";
                registro.flags <= "000";
            else

                -- Decodificación de la instrucción

                registro.ir <= memoria(registro.pc);

                case registro.ir is
                    when opcode_add =>
                        registro.ac <= registro.ac + memoria(registro.mar);
                    when opcode_sub =>
                        registro.ac <= registro.ac - memoria(registro.mar);
                    when opcode_and =>
                        registro.ac <= registro.ac and memoria(registro.mar);
                    when opcode_or =>
                        registro.ac <= registro.ac or memoria(registro.mar);
                    when opcode_not =>
                        registro.ac <= not registro.ac;
                    when opcode_ld =>
                        registro.ac <= memoria(registro.mar);
                    when opcode_st =>
                        memoria(registro.mar) <= registro.ac;
                    when opcode_jmp =>
                        registro.pc <= memoria(registro.mar);
                    when opcode_je =>
                        if registro.flags(0) = '1' then
                            registro.pc <= memoria(registro.mar);
                        end if;
                    when opcode_jne =>
                        if registro.flags(0) = '0' then
                            registro.pc <= memoria(registro.mar);
                        end if;
                    when opcode_jl =>
                        if registro.flags(1) = '1' then
                            registro.pc <= memoria(registro.mar);