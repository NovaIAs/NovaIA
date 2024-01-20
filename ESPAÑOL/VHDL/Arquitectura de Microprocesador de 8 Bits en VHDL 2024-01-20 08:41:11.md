```vhdl
-- Un código complejo en VHDL que implementa una arquitectura de microprocesador de 8 bits:

-- Nombre del módulo: microprocesador

entidad microprocesador es
    -- Definición de las señales de entrada y salida del microprocesador
    puerto: in std_logic_vector(7 downto 0);
    instruccion: in std_logic_vector(7 downto 0);
    reloj: in std_logic;
    reinicio: in std_logic;

    accumulator: out std_logic_vector(7 downto 0);
    banderas: out std_logic_vector(3 downto 0);
    direccion_memoria: out std_logic_vector(11 downto 0);
    datos_memoria: in std_logic_vector(7 downto 0);
end entidad;

arquitectura comportamiento de microprocesador es
    -- Definición de los registros internos del microprocesador
    contador_programa: unsigned(11 downto 0);
    registro_instrucciones: std_logic_vector(7 downto 0);
    registro_acumulador: std_logic_vector(7 downto 0);
    registro_banderas: std_logic_vector(3 downto 0);
    registro_direccion_memoria: std_logic_vector(11 downto 0);
    registro_datos_memoria: std_logic_vector(7 downto 0);

    -- Definición de las etapas del ciclo de instrucción
    etapa: std_logic_vector(2 downto 0);

    -- Definición de las constantes de las instrucciones
    const LOAD: std_logic_vector(7 downto 0) := "00000000";
    const ADD: std_logic_vector(7 downto 0) := "00000001";
    const SUB: std_logic_vector(7 downto 0) := "00000010";
    const AND: std_logic_vector(7 downto 0) := "00000011";
    const OR: std_logic_vector(7 downto 0) := "00000100";
    const XOR: std_logic_vector(7 downto 0) := "00000101";
    const CMP: std_logic_vector(7 downto 0) := "00000110";
    const JMP: std_logic_vector(7 downto 0) := "00000111";
    const JZ: std_logic_vector(7 downto 0) := "00001000";
    const JNZ: std_logic_vector(7 downto 0) := "00001001";
    const JC: std_logic_vector(7 downto 0) := "00001010";
    const JNC: std_logic_vector(7 downto 0) := "00001011";
    const RET: std_logic_vector(7 downto 0) := "00001100";
    const HALT: std_logic_vector(7 downto 0) := "00001101";

    -- Procedimiento para la etapa de búsqueda de instrucción
    procedimiento busqueda_instruccion es
    begin
        if reloj'event and reloj = '1' then
            registro_instrucciones <= puerto;
            contador_programa <= contador_programa + 1;
        end if;
    end procedimiento;

    -- Procedimiento para la etapa de extracción de instrucción
    procedimiento extraccion_instruccion es
    begin
        case etapa(0) is
        when '0' =>
            if reloj'event and reloj = '1' then
                registro_direccion_memoria <= contador_programa;
                etapa <= etapa + 1;
            end if;
        when '1' =>
            if datos_memoria'valid then
                registro_instrucciones <= datos_memoria;
                etapa <= etapa + 1;
            end if;
        end case;
    end procedimiento;

    -- Procedimiento para la etapa de ejecución de instrucción
    procedimiento ejecucion_instruccion es
    begin
        case registro_instrucciones(7 downto 4) is
        when LOAD =>
            registro_acumulador <= puerto;
            etapa <= '000';
        when ADD =>
            registro_acumulador <= registro_acumulador + puerto;
            etapa <= '000';
        when SUB =>
            registro_acumulador <= registro_acumulador - puerto;
            etapa <= '000';
        when AND =>
            registro_acumulador <= registro_acumulador and puerto;
            etapa <= '000';
        when OR =>
            registro_acumulador <= registro_acumulador or puerto;
            etapa <= '000';
        when XOR =>
            registro_acumulador <= registro_acumulador xor puerto;
            etapa <= '000';
        when CMP =>
            registro_banderas(0) <= '1' if registro_acumulador = puerto else '0';
            registro_banderas(1) <= '1' if registro_acumulador < puerto else '0';
            registro_banderas(2) <= '1' if registro_acumulador > puerto else '0';
            registro_banderas(3) <= '1' if registro_acumulador = 0 else '0';
            etapa <= '000';
        when JMP =>
            contador_programa <= std_logic_vector(puerto(3 downto 0) & "0000");
            etapa <= '000';
        when JZ =>
            if registro_banderas(3) = '1' then
                contador_programa <= std_logic_vector(puerto(3 downto 0) & "0000");
            end if;
            etapa <= '000';
        when JNZ =>
            if registro_banderas(3) = '0' then
                contador_programa <= std_logic_vector(puerto(3 downto 0) & "0000");
            end if;
            etapa <= '000';