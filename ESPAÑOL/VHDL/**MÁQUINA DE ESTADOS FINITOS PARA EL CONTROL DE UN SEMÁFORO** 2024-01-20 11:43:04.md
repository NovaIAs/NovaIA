```vhdl
-- Este es un código VHDL complejo que implementa una máquina de estados finitos (FSM) para controlar un semáforo.

-- Declaración de la entidad
entity semaforo is
    port (
        clk : in std_logic; -- Reloj
        reset : in std_logic; -- Reinicio
        rojo : out std_logic; -- Señal de luz roja
        amarillo : out std_logic; -- Señal de luz amarilla
        verde : out std_logic -- Señal de luz verde
    );
end semaforo;

-- Declaración de la arquitectura
architecture behavioral of semaforo is
    -- Estados de la FSM
    type state_type is (S0, S1, S2, S3);
    signal state : state_type := S0; -- Estado actual

    -- Temporizadores
    signal rojo_timer : integer range 0 to 10000 := 0; -- Temporizador de luz roja
    signal amarillo_timer : integer range 0 to 1000 := 0; -- Temporizador de luz amarilla
    signal verde_timer : integer range 0 to 10000 := 0; -- Temporizador de luz verde

    -- Lógica combinacional
    rojo <= '1' when state = S0 else '0'; -- Señal de luz roja activa en el estado S0
    amarillo <= '1' when state = S1 else '0'; -- Señal de luz amarilla activa en el estado S1
    verde <= '1' when state = S2 else '0'; -- Señal de luz verde activa en el estado S2

    -- Lógica secuencial
    process (clk, reset)
    begin
        if reset = '1' then -- Reinicio
            state <= S0;
            rojo_timer <= 0;
            amarillo_timer <= 0;
            verde_timer <= 0;
        elsif rising_edge(clk) then -- Flanco ascendente del reloj
            case state is
                when S0 =>
                    if rojo_timer = 10000 then -- Si el temporizador de luz roja ha expirado
                        state <= S1; -- Pasar al estado S1
                        rojo_timer <= 0; -- Restablecer el temporizador de luz roja
                        amarillo_timer <= 1000; -- Iniciar el temporizador de luz amarilla
                    else
                        rojo_timer <= rojo_timer + 1; -- Incrementar el temporizador de luz roja
                    end if;
                when S1 =>
                    if amarillo_timer = 0 then -- Si el temporizador de luz amarilla ha expirado
                        state <= S2; -- Pasar al estado S2
                        amarillo_timer <= 0; -- Restablecer el temporizador de luz amarilla
                        verde_timer <= 10000; -- Iniciar el temporizador de luz verde
                    else
                        amarillo_timer <= amarillo_timer - 1; -- Decrementar el temporizador de luz amarilla
                    end if;
                when S2 =>
                    if verde_timer = 0 then -- Si el temporizador de luz verde ha expirado
                        state <= S0; -- Pasar al estado S0
                        verde_timer <= 0; -- Restablecer el temporizador de luz verde
                        rojo_timer <= 10000; -- Iniciar el temporizador de luz roja
                    else
                        verde_timer <= verde_timer - 1; -- Decrementar el temporizador de luz verde
                    end if;
            end case;
        end if;
    end process;
end behavioral;
```

**Explicación del código:**

* La entidad `semaforo` define los puertos de entrada y salida del semáforo.
* La arquitectura `behavioral` implementa la lógica del semáforo.
* La FSM tiene cuatro estados: `S0` (rojo), `S1` (amarillo), `S2` (verde) y `S3` (rojo intermitente).
* Tres temporizadores llevan la cuenta del tiempo que cada luz permanece encendida.
* La lógica combinacional activa las señales de luz Roja, amarilla y verde en función del estado actual.
* La lógica secuencial se encarga de cambiar el estado de la FSM en función del tiempo transcurrido.