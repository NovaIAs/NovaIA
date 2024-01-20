```vhdl
-- Este código es un ejemplo de un código complejo en VHDL.
-- El código implementa un controlador de semáforos.
-- El código está dividido en varias partes:

-- 1. La declaración de la entidad:

entity controlador_semaforos is
    port (
        -- Señales de entrada
        clk : in std_logic;
        reset : in std_logic;
        boton_peaton : in std_logic;

        -- Señales de salida
        semaforo_coches : out std_logic_vector(1 downto 0);
        semaforo_peatones : out std_logic
    );
end entity controlador_semaforos;

-- 2. La declaración de la arquitectura:

architecture comportamiento of controlador_semaforos is

    -- Declaración de constantes y tipos:

    constant TIEMPO_VERDE_COCHES : integer := 100;
    constant TIEMPO_AMARILLO_COCHES : integer := 5;
    constant TIEMPO_ROJO_COCHES : integer := 100;
    constant TIEMPO_VERDE_PEATONES : integer := 15;
    constant TIEMPO_ROJO_PEATONES : integer := 100;

    type estado_semaforos is (
        VERDE_COCHES,
        AMARILLO_COCHES,
        ROJO_COCHES,
        VERDE_PEATONES,
        ROJO_PEATONES
    );

    -- Declaración del estado actual:

    signal estado_actual : estado_semaforos := VERDE_COCHES;

    -- Declaración del contador:

    signal contador : integer range 0 to 200 := 0;

    -- Declaración de los procesos:

    process (clk)
    begin
        if reset = '1' then
            estado_actual <= VERDE_COCHES;
            contador <= 0;
        elsif rising_edge(clk) then
            case estado_actual is
                when VERDE_COCHES =>
                    if contador = TIEMPO_VERDE_COCHES then
                        estado_actual <= AMARILLO_COCHES;
                        contador <= 0;
                    else
                        contador <= contador + 1;
                    end if;
                when AMARILLO_COCHES =>
                    if contador = TIEMPO_AMARILLO_COCHES then
                        estado_actual <= ROJO_COCHES;
                        contador <= 0;
                    else
                        contador <= contador + 1;
                    end if;
                when ROJO_COCHES =>
                    if boton_peaton = '1' then
                        estado_actual <= VERDE_PEATONES;
                        contador <= 0;
                    elsif contador = TIEMPO_ROJO_COCHES then
                        estado_actual <= VERDE_COCHES;
                        contador <= 0;
                    else
                        contador <= contador + 1;
                    end if;
                when VERDE_PEATONES =>
                    if contador = TIEMPO_VERDE_PEATONES then
                        estado_actual <= ROJO_PEATONES;
                        contador <= 0;
                    else
                        contador <= contador + 1;
                    end if;
                when ROJO_PEATONES =>
                    if contador = TIEMPO_ROJO_PEATONES then
                        estado_actual <= VERDE_COCHES;
                        contador <= 0;
                    else
                        contador <= contador + 1;
                    end if;
            end case;
        end if;
    end process;

    process (estado_actual)
    begin
        case estado_actual is
            when VERDE_COCHES =>
                semaforo_coches <= "01";
                semaforo_peatones <= '0';
            when AMARILLO_COCHES =>
                semaforo_coches <= "10";
                semaforo_peatones <= '0';
            when ROJO_COCHES =>
                semaforo_coches <= "11";
                semaforo_peatones <= '0';
            when VERDE_PEATONES =>
                semaforo