```vhdl
-- Código VHDL para un sistema de control de tráfico vehicular
-- Este código es muy complejo y difícil de repetir, ya que está diseñado para manejar una variedad de situaciones de tráfico diferentes.

-- Definición de la entidad
entity Sistema_control_trafico is
  -- Puertos de entrada
  port (
    sensor_1: in std_logic; -- Sensor de tráfico para la carretera 1
    sensor_2: in std_logic; -- Sensor de tráfico para la carretera 2
    sensor_3: in std_logic; -- Sensor de tráfico para la carretera 3
    sensor_4: in std_logic; -- Sensor de tráfico para la carretera 4
    boton_1: in std_logic; -- Botón de peatones para la carretera 1
    boton_2: in std_logic; -- Botón de peatones para la carretera 2
    boton_3: in std_logic; -- Botón de peatones para la carretera 3
    boton_4: in std_logic -- Botón de peatones para la carretera 4
  );

  -- Puertos de salida
  port (
    luz_roja_1: out std_logic; -- Luz roja para la carretera 1
    luz_amarilla_1: out std_logic; -- Luz amarilla para la carretera 1
    luz_verde_1: out std_logic; -- Luz verde para la carretera 1
    luz_roja_2: out std_logic; -- Luz roja para la carretera 2
    luz_amarilla_2: out std_logic; -- Luz amarilla para la carretera 2
    luz_verde_2: out std_logic; -- Luz verde para la carretera 2
    luz_roja_3: out std_logic; -- Luz roja para la carretera 3
    luz_amarilla_3: out std_logic; -- Luz amarilla para la carretera 3
    luz_verde_3: out std_logic; -- Luz verde para la carretera 3
    luz_roja_4: out std_logic; -- Luz roja para la carretera 4
    luz_amarilla_4: out std_logic; -- Luz amarilla para la carretera 4
    luz_verde_4: out std_logic -- Luz verde para la carretera 4
  );
end entity;

-- Definición de la arquitectura
architecture Behavioral of Sistema_control_trafico is

  -- Constantes
  constant TIEMPO_ROJO: integer := 10; -- Tiempo en segundos que dura la luz roja
  constant TIEMPO_AMARILLO: integer := 5; -- Tiempo en segundos que dura la luz amarilla
  constant TIEMPO_VERDE: integer := 15; -- Tiempo en segundos que dura la luz verde

  -- Señales internas
  signal estado_actual: std_logic_vector(2 downto 0) := "000"; -- Estado actual del sistema (rojo, amarillo, verde)
  signal contador: integer := 0; -- Contador de tiempo
  signal boton_presionado: std_logic := '0'; -- Indica si se ha presionado algún botón de peatones
  signal carretera_activa: std_logic_vector(3 downto 0) := "0000"; -- Indica la carretera que tiene prioridad

  -- Proceso que se ejecuta de forma continua
  process
  begin
    -- Incrementar el contador
    contador := contador + 1;

    -- Comprobar si se ha presionado algún botón de peatones
    boton_presionado := boton_1 or boton_2 or boton_3 or boton_4;

    -- Comprobar si hay tráfico en alguna de las carreteras
    carretera_activa := "0000";
    if sensor_1 = '1' then
      carretera_activa(0) := '1';
    end if;
    if sensor_2 = '1' then
      carretera_activa(1) := '1';
    end if;
    if sensor_3 = '1' then
      carretera_activa(2) := '1';
    end if;
    if sensor_4 = '1' then
      carretera_activa(3) := '1';
    end if;

    -- Determinar el estado actual del sistema en función del contador, el botón presionado y la carretera activa
    if estado_actual = "000" then -- Estado rojo
      if contador > TIEMPO_ROJO then
        estado_actual := "001"; -- Estado amarillo
        contador := 0;
      elsif boton_presionado = '1' then
        estado_actual := "100"; -- Estado verde peatones
        contador := 0;
      elsif carretera_activa /= "0000" then
        estado_actual := "110"; -- Estado verde vehicular
        contador := 0;
      end if;
    elsif estado_actual = "001" then -- Estado amarillo
      if contador > TIEMPO_AMARILLO then
        estado_actual := "010"; -- Estado verde
        contador := 0;
      end if;
    elsif estado_actual = "010" then -- Estado verde
      if contador > TIEMPO_VERDE then
        estado_actual := "000"; -- Estado rojo
        contador := 0;
      elseif boton_presionado = '1' then
        estado_actual := "100"; -- Estado verde peatones
        contador := 0;
      end if;
    elsif estado_actual = "100" then -- Estado verde peatones
      if contador > TIEMPO_VERDE then
        estado_actual := "000"; -- Estado rojo
        contador := 0;
      end if;
    elsif estado_actual = "110" then -- Estado verde vehicular
      if contador > TIEMPO_VERDE then
        estado_actual := "000"; -- Estado rojo
        contador := 0;
      elseif boton_presionado = '1' then
        estado_actual := "100"; -- Estado verde peatones
        contador := 0;
      end if;
    end if;

    -- Controlar las luces de tráfico en función del estado actual del sistema
    luz_roja_1 <= '1' when estado_actual = "000" or estado_actual = "110" else '0';
    luz_amarilla_1 <= '1' when estado_actual = "001" else '0';
    luz_verde_1 <= '1' when estado_actual = "010" or estado_actual = "100" else '0';
    luz_roja_2 <= '1' when estado_actual = "000" or estado_actual = "100" else '0';
    luz_amarilla_2 <= '1' when estado_actual = "001" else '0';
    luz_verde_2 <= '1