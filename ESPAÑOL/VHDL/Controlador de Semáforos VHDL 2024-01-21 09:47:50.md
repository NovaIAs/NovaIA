```vhdl
-- Código VHDL para un controlador de semáforos

-- Definición de los tipos de señal
type Estado_semaforo is (Rojo, Verde, Amarillo);
type Direccion_coche is (Norte, Este, Sur, Oeste);

-- Definición de la interfaz del módulo
entity controlador_semaforos is
    port(
        -- Señales de entrada
        clk: in std_logic;        -- Reloj del sistema
        btn_norte: in std_logic;   -- Botón de peatones norte
        btn_este: in std_logic;    -- Botón de peatones este
        btn_sur: in std_logic;     -- Botón de peatones sur
        btn_oeste: in std_logic;   -- Botón de peatones oeste
        dir_coche: in Direccion_coche     -- Dirección del coche en movimiento

        -- Señales de salida
        semaforo_norte: out std_logic;    -- Señal del semáforo norte
        semaforo_este: out std_logic;     -- Señal del semáforo este
        semaforo_sur: out std_logic;      -- Señal del semáforo sur
        semaforo_oeste: out std_logic     -- Señal del semáforo oeste
    );
end controlador_semaforos;

-- Arquitectura del módulo
architecture comportamiento of controlador_semaforos is

    -- Definición de los estados del semáforo
    constant Rojo_Norte : Estado_semaforo := Rojo;
    constant Verde_Norte : Estado_semaforo := Verde;
    constant Amarillo_Norte : Estado_semaforo := Amarillo;
    constant Rojo_Este : Estado_semaforo := Rojo;
    constant Verde_Este : Estado_semaforo := Verde;
    constant Amarillo_Este : Estado_semaforo := Amarillo;
    constant Rojo_Sur : Estado_semaforo := Rojo;
    constant Verde_Sur : Estado_semaforo := Verde;
    constant Amarillo_Sur : Estado_semaforo := Amarillo;
    constant Rojo_Oeste : Estado_semaforo := Rojo;
    constant Verde_Oeste : Estado_semaforo := Verde;
    constant Amarillo_Oeste : Estado_semaforo := Amarillo;

    -- Definición de las transiciones de estados del semáforo
    constant transiciones_semaforos : array(Estado_semaforo, Direccion_coche) of Estado_semaforo is ((
        (Rojo_Norte, Norte) => Verde_Norte,
        (Rojo_Norte, Este) => Amarillo_Norte,
        (Rojo_Norte, Sur) => Rojo_Norte,
        (Rojo_Norte, Oeste) => Rojo_Norte,

        (Verde_Norte, Norte) => Rojo_Norte,
        (Verde_Norte, Este) => Verde_Norte,
        (Verde_Norte, Sur) => Rojo_Norte,
        (Verde_Norte, Oeste) => Rojo_Norte,

        (Amarillo_Norte, Norte) => Amarillo_Norte,
        (Amarillo_Norte, Este) => Rojo_Este,
        (Amarillo_Norte, Sur) => Rojo_Norte,
        (Amarillo_Norte, Oeste) => Rojo_Norte,

        (Rojo_Este, Norte) => Rojo_Este,
        (Rojo_Este, Este) => Verde_Este,
        (Rojo_Este, Sur) => Rojo_Este,
        (Rojo_Este, Oeste) => Rojo_Este,

        (Verde_Este, Norte) => Rojo_Este,
        (Verde_Este, Este) => Rojo_Este,
        (Verde_Este, Sur) => Rojo_Este,
        (Verde_Este, Oeste) => Verde_Este,

        (Amarillo_Este, Norte) => Rojo_Este,
        (Amarillo_Este, Este) => Amarillo_Este,
        (Amarillo_Este, Sur) => Rojo_Este,
        (Amarillo_Este, Oeste) => Rojo_Este,

        (Rojo_Sur, Norte) => Rojo_Sur,
        (Rojo_Sur, Este) => Rojo_Sur,
        (Rojo_Sur, Sur) => Verde_Sur,
        (Rojo_Sur, Oeste) => Rojo_Sur,

        (Verde_Sur, Norte) => Rojo_Sur,
        (Verde_Sur, Este) => Rojo_Sur,
        (Verde_Sur, Sur) => Rojo_Sur,
        (Verde_Sur, Oeste) => Verde_Sur,

        (Amarillo_Sur, Norte) => Rojo_Sur,
        (Amarillo_Sur, Este) => Rojo_Sur,
        (Amarillo_Sur, Sur) => Amarillo_Sur,
        (Amarillo_Sur, Oeste) => Rojo_Sur,

        (Rojo_Oeste, Norte) => Rojo_Oeste,
        (Rojo_Oeste, Este) => Rojo_Oeste,
        (Rojo_Oeste, Sur) => Rojo_Oeste,
        (Rojo_Oeste, Oeste) => Verde_Oeste,

        (Verde_Oeste, Norte) => Rojo_Oeste,
        (Verde_Oeste, Este) => Rojo_Oeste,
        (Verde_Oeste, Sur) => Rojo_Oeste,
        (Verde_Oeste, Oeste) => Rojo_Oeste,

        (Amarillo_Oeste, Norte) => Rojo_Oeste,
        (Amarillo_Oeste, Este) => Rojo_Oeste,
        (Amarillo_Oeste, Sur) => Rojo_Oeste,
        (Amarillo_Oeste, Oeste) => Amarillo_Oeste
    ));


    -- Definición de los registros de estado
    signal estado_semaforo_norte : Estado_semaforo := Rojo;
    signal estado_semaforo_este : Estado_semaforo := Rojo;
    signal estado_semaforo_sur : Estado_semaforo := Rojo;
    signal estado_semaforo_oeste : Estado_semaforo := Rojo;

    -- Proceso para actualizar el estado del semáforo
    process(clk) begin
        if rising_edge(clk) then
            -- Obtener la nueva dirección del coche en movimiento
            direccion_coche := dir_coche;

            -- Obtener el nuevo estado del semáforo para cada dirección
            estado_semaforo_norte := transiciones_semaforos(estado_semaforo_norte, direccion_coche);
            estado_semaforo_este := transiciones_semaforos(estado_semaforo_este, direccion_coche);
            estado_semaforo_sur := transiciones_semaforos(estado_semaforo_sur, direccion_coche);
            estado_semaforo_oeste := transiciones_semaforos(estado_semaforo_oeste, direccion_coche);
        end if;