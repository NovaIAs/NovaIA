```vhdl
-- Nombre del código: "Controlador de Semáforo Complejo"

-- Librerías a utilizar
library IEEE;
use IEEE.std_logic_1164.all;

-- Entidad
entity ControladorSemaforoComplejo is
    port(
        clk_200MHz : in std_logic;        -- Señal de reloj de 200 MHz proveniente de un generador de reloj
        btn_rojo : in std_logic;          -- Botón para activar la señal Roja del semáforo manualmente
        btn_amarillo : in std_logic;      -- Botón para activar la señal Amarilla del semáforo manualmente
        btn_verde : in std_logic;         -- Botón para activar la señal Verde del semáforo manualmente
        btn_SOS : in std_logic;           -- Botón de emergencia para detener todos los ciclos y mostrar Rojo intermitente
        sw1 : in std_logic;              -- Selector de modo de operación (automático o manual)
        sw2 : in std_logic;              -- Selector de duración del ciclo entre Rojo, Amarillo y Verde
        led_rojo_manual : out std_logic;  -- Señal de salida para activar la luz Roja del semáforo manualmente
        led_amarillo_manual : out std_logic; -- Señal de salida para activar la luz Amarilla del semáforo manualmente
        led_verde_manual : out std_logic; -- Señal de salida para activar la luz Verde del semáforo manualmente
        led_rojo_auto : out std_logic;    -- Señal de salida para activar la luz Roja del semáforo en modo automático
        led_amarillo_auto : out std_logic; -- Señal de salida para activar la luz Amarilla del semáforo en modo automático
        led_verde_auto : out std_logic;   -- Señal de salida para activar la luz Verde del semáforo en modo automático
        led_intermitente_rojo : out std_logic -- Señal de salida para activar la luz Roja intermitente en caso de emergencia
    );
end ControladorSemaforoComplejo;

-- Arquitectura
architecture SemaforoComplejo of ControladorSemaforoComplejo is
    -- Variables
    type EstadoSemaforo is (Rojo, Amarillo, Verde, RojoIntermitente);
    signal estado_semaforo : EstadoSemaforo := Rojo;
    signal contador : unsigned(7 downto 0) := (others => '0');
    signal periodo_ciclo : unsigned(7 downto 0) := (others => '0');
    constant Rojo_Duracion : unsigned(7 downto 0) := x"00";
    constant Amarillo_Duracion : unsigned(7 downto 0) := x"0A";
    constant Verde_Duracion : unsigned(7 downto 0) := x"1E";

begin
    -- Proceso para manejar el reloj de 200 MHz
    process(clk_200MHz)
    begin
        if rising_edge(clk_200MHz) then
            if contador < 199 then
                contador <= contador + 1;
            else
                contador <= (others => '0');
            end if;
        end if;
    end process;

    -- Proceso para el selector de modo automático o manual
    process(sw1)
    begin
        if sw1 = '0' then
            -- Modo automático
            estado_semaforo <= Rojo;
        elsif sw1 = '1' then
            -- Modo manual
            if btn_rojo = '1' then
                estado_semaforo <= Rojo;
            elsif btn_amarillo = '1' then
                estado_semaforo <= Amarillo;
            elsif btn_verde = '1' then
                estado_semaforo <= Verde;
            end if;
        end if;
    end process;

    -- Proceso para el selector de duración del ciclo
    process(sw2)
    begin
        if sw2 = '0' then
            -- Ciclo corto
            periodo_ciclo <= Rojo_Duracion;
        elsif sw2 = '1' then
            -- Ciclo medio
            periodo_ciclo <= Amarillo_Duracion;
        elsif sw2 = '2' then
            -- Ciclo largo
            periodo_ciclo <= Verde_Duracion;
        end if;
    end process;

    -- Proceso para el ciclo del semáforo
    process(contador, estado_semaforo)
    begin
        if contador < periodo_ciclo then
            -- Durante el tiempo de espera
            case estado_semaforo is
                when Rojo =>
                    led_rojo_auto <= '1';
                    led_amarillo_auto <= '0';
                    led_verde_auto <= '0';
                when Amarillo =>
                    led_rojo_auto <= '0';
                    led_amarillo_auto <= '1';
                    led_verde_auto <= '0';
                when Verde =>
                    led_rojo_auto <= '0';
                    led_amarillo_auto <= '0';
                    led_verde_auto <= '1';
            end case;
        elsif contador >= periodo_ciclo then
            -- Al finalizar el tiempo de espera
            if estado_semaforo = Rojo then
                estado_semaforo <= Amarillo;
            elsif estado_semaforo = Amarillo then
                estado_semaforo <= Verde;
            elsif estado_semaforo = Verde then
                estado_semaforo <= Rojo;
            end if;
        end if;
    end process;

    -- Proceso para el botón de emergencia (SOS)
    process(btn_SOS)
    begin
        if btn_SOS = '1' then
            estado_semaforo <= RojoIntermitente;
        end if;
    end process;

    -- Proceso para la luz roja intermitente
    process(estado_semaforo)
    begin
        if estado_semaforo = RojoIntermitente then
            led_rojo_auto <= not led_rojo_auto;
            led_amarillo_auto <= '0';
            led_verde_auto <= '0';
            led_intermitente_rojo <= '1';
        else
            led_intermitente_rojo <= '0';
        end if;
    end process;

    -- Señales de salida del semáforo
    led_rojo_manual <= (estado_semaforo = Rojo) and (sw1 = '1');
    led_amarillo_manual <= (estado_semaforo = Amarillo) and (sw1 = '1');
    led_verde_manual <= (estado_semaforo = Verde) and (sw1 = '1');
end SemaforoComplejo;
```

Explicación del código:

* **Entidad:** Define los puertos de entrada y salida del módulo, así como su tipo.
* **Arquitectura:** Define la implementación del módulo, incluyendo los procesos y las variables.
* **Procesos:** Hay varios procesos en el código, cada uno con su propia tarea específica.
    * El proceso `clk_200MHz` maneja el reloj de 200 MHz y cuenta los ticks del reloj.
    * El proceso `sw1` maneja el selector de modo automático o manual.
    * El proceso `sw2` maneja el selector de duración del ciclo del semáforo.
    * El proceso `contador` maneja el ciclo del semáforo en modo automático.
    * El proceso `btn_SOS` maneja el botón de emergencia.
    * El proceso `estado_semaforo` maneja la luz roja intermitente.
* **Señales de salida:** Las señales de salida del semáforo se asignan en función del estado actual del semáforo y los selectores de modo.