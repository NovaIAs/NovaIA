```vhdl
-- Modelo VHDL de un sistema de control de tráfico
-- Por F. Sánchez y A. García

-- Declaración de entidades
-- ===========================================================================

-- Semáforo
entity Semaforo is
    port
        reloj    : in std_logic; -- Señal de reloj
        peticion : in std_logic; -- Señal de petición de cruce
        cruce    : out std_logic; -- Señal de cruce
        rojo     : out std_logic; -- Señal de luz roja
        verde    : out std_logic; -- Señal de luz verde
    end entity;

-- Manejador de eventos
entity ManejadorEventos is
    port
        reloj        : in std_logic; -- Señal de reloj
        peticion     : in std_logic; -- Señal de petición de cruce
        cruce        : out std_logic; -- Señal de cruce
        deteccion_1  : in std_logic; -- Señal de detección de vehículo en el carril 1
        deteccion_2  : in std_logic; -- Señal de detección de vehículo en el carril 2
    end entity;

-- Controlador de semáforos
entity ControladorSemaforos is
    port
        reloj       : in std_logic; -- Señal de reloj
        cruce1      : in std_logic; -- Señal de cruce del semáforo 1
        cruce2      : in std_logic; -- Señal de cruce del semáforo 2
        deteccion1  : in std_logic; -- Señal de detección de vehículo en el carril 1
        deteccion2  : in std_logic; -- Señal de detección de vehículo en el carril 2
        rojo1       : out std_logic; -- Señal de luz roja del semáforo 1
        verde1      : out std_logic; -- Señal de luz verde del semáforo 1
        rojo2       : out std_logic; -- Señal de luz roja del semáforo 2
        verde2      : out std_logic; -- Señal de luz verde del semáforo 2
    end entity;


-- Declaración de arquitecturas
-- ===========================================================================

-- Arquitectura del semáforo
architecture SEMAFORO of Semaforo is
    -- Variables
    signal estado : std_logic_vector(2 downto 0) := "000"; -- Estado actual del semáforo
    signal rojo_aux, verde_aux : std_logic := '0'; -- Variables auxiliares para las señales de rojo y verde

begin
    -- Proceso de control del semáforo
    process(reloj)
    begin
        if rising_edge(reloj) then
            case estado is
                when "000" => -- Estado inicial
                    if peticion = '1' then -- Se ha solicitado el cruce
                        estado <= "001"; -- Pasar al estado de espera
                    else
                        estado <= "000"; -- Mantener el estado actual
                    end if;
                when "001" => -- Estado de espera
                    if cruce = '0' then -- No se ha autorizado el cruce
                        estado <= "002"; -- Pasar al estado de rojo
                    else
                        estado <= "001"; -- Mantener el estado actual
                    end if;
                when "002" => -- Estado de rojo
                    rojo_aux <= '1'; -- Activar la señal de rojo
                    verde_aux <= '0'; -- Desactivar la señal de verde
                    after 500 ms -- Esperar 500 ms
                        estado <= "003"; -- Pasar al estado de verde
                    end if;
                when "003" => -- Estado de verde
                    rojo_aux <= '0'; -- Desactivar la señal de rojo
                    verde_aux <= '1'; -- Activar la señal de verde
                    after 500 ms -- Esperar 500 ms
                        estado <= "000"; -- Pasar al estado inicial
                    end if;
            end case;
        end if;
    end process;

    -- Asignación de las señales de salida
    cruce   <= estado(0); -- Señal de cruce
    rojo    <= rojo_aux; -- Señal de luz roja
    verde   <= verde_aux; -- Señal de luz verde
end architecture SEMAFORO;

-- Arquitectura del manejador de eventos
architecture MANEJADOREVENTES of ManejadorEventos is
    -- Variables
    signal cruce_aux : std_logic := '0'; -- Variable auxiliar para la señal de cruce

begin
    -- Proceso de control del manejador de eventos
    process(reloj)
    begin
        if rising_edge(reloj) then
            if peticion = '1' then -- Se ha solicitado el cruce
                cruce_aux <= '1'; -- Autorizar el cruce
            else
                cruce_aux <= '0'; -- No autorizar el cruce
            end if;

            if deteccion_1 = '0' and deteccion_2 = '0' then -- No hay vehículos en los carriles
                cruce_aux <= '0'; -- No autorizar el cruce
            end if;
        end if;
    end process;

    -- Asignación de la señal de salida
    cruce <= cruce_aux; -- Señal de cruce
end architecture MANEJADOREVENTES;

-- Arquitectura del controlador de semáforos
architecture CONTROLADORSE