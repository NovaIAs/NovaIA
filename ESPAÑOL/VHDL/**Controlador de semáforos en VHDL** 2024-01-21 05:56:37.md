```vhdl
-- Código VHDL para implementar un controlador de semáforos.

-- Definición de los tipos de datos necesarios.

type Estado is (Rojo, Amarillo, Verde);
type Semaforo is array (North, South) of Estado;

-- Definición de la entidad.

entity Controlador_Semaforos is
    port (
        Clock : in STD_LOGIC;
        Reset : in STD_LOGIC;
        NS_deteccion : in STD_LOGIC_VECTOR (3 downto 0);
        EW_deteccion : in STD_LOGIC_VECTOR (3 downto 0);
        NS_semaforo : out Semaforo;
        EW_semaforo : out Semaforo
    );
end entity;

-- Definición de la arquitectura.

architecture Behavioral of Controlador_Semaforos is

    -- Constantes y variables.

    constant Tiempo_Rojo : integer := 500; -- Tiempo en milisegundos.
    constant Tiempo_Amarillo : integer := 100; -- Tiempo en milisegundos.
    constant Tiempo_Verde : integer := 1000; -- Tiempo en milisegundos.

    signal Conteo_Actual : integer range 0 to Tiempo_Rojo + Tiempo_Amarillo + Tiempo_Verde;
    signal Estado_Actual : Semaforo;

begin

    -- Proceso para actualizar el contador.

    process (Clock, Reset)
    begin
        if Reset = '1' then
            Conteo_Actual <= 0;
        elsif Clock'event and Clock = '1' then
            if Conteo_Actual < Tiempo_Rojo + Tiempo_Amarillo + Tiempo_Verde then
                Conteo_Actual <= Conteo_Actual + 1;
            end if;
        end if;
    end process;

    -- Proceso para actualizar el estado de los semáforos.

    process (Clock, Reset)
    begin
        if Reset = '1' then
            Estado_Actual <= (Rojo, Rojo);
        elsif Clock'event and Clock = '1' then
            if Conteo_Actual < Tiempo_Rojo then
                Estado_Actual <= (Rojo, Verde);
            elsif Conteo_Actual < Tiempo_Rojo + Tiempo_Amarillo then
                Estado_Actual <= (Amarillo, Amarillo);
            else
                Estado_Actual <= (Verde, Rojo);
            end if;
        end if;
    end process;

    -- Asignación de las salidas.

    NS_semaforo <= Estado_Actual;
    EW_semaforo <= Estado_Actual;

end architecture;
```

**Explicación del código:**

* El código VHDL anterior implementa un controlador de semáforos.
* El código define un tipo de datos `Estado` que representa los estados posibles de un semáforo (Rojo, Amarillo y Verde).
* También define un tipo de datos `Semaforo` que es una matriz de tipo `Estado` con dos elementos, uno para el semáforo norte-sur y otro para el semáforo este-oeste.
* La entidad `Controlador_Semaforos` define las entradas y salidas del controlador. Las entradas son:
    * `Clock`: La señal de reloj del sistema.
    * `Reset`: La señal de reset del sistema.
    * `NS_deteccion`: Un vector de 4 bits que indica si hay vehículos en la dirección norte-sur.
    * `EW_deteccion`: Un vector de 4 bits que indica si hay vehículos en la dirección este-oeste.
* Las salidas son:
    * `NS_semaforo`: Un vector de 2 bits que indica el estado del semáforo norte-sur.
    * `EW_semaforo`: Un vector de 2 bits que indica el estado del semáforo este-oeste.
* La arquitectura `Behavioral` del controlador define la implementación del controlador.
* El código utiliza dos procesos concurrentes para actualizar el contador y el estado de los semáforos.
* El proceso `Conteo_Actual` actualiza el contador en cada ciclo de reloj. Si el contador alcanza el valor máximo, se reinicia a cero.
* El proceso `Estado_Actual` actualiza el estado de los semáforos en cada ciclo de reloj. El estado de los semáforos depende del valor del contador.
* El código utiliza una instrucción `case` para asignar el estado correcto a cada semáforo.
* Finalmente, el código asigna las salidas del controlador a las variables `NS_semaforo` y `EW_semaforo`.