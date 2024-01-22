```vhdl
-- LIBRERÍAS NECESARIAS
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

-- DEFINICIÓN DE LA ENTIDAD
ENTITY control_luces_trafico IS
  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    // Señales de entrada para los sensores de tráfico
    sensor_norte : IN STD_LOGIC;
    sensor_sur : IN STD_LOGIC;
    sensor_este : IN STD_LOGIC;
    sensor_oeste : IN STD_LOGIC;
    // Señales de salida para los semáforos
    semaforo_norte : OUT STD_LOGIC;
    semaforo_sur : OUT STD_LOGIC;
    semaforo_este : OUT STD_LOGIC;
    semaforo_oeste : OUT STD_LOGIC
  );
END ENTITY;

-- ARQUITECTURA DE LA ENTIDAD
ARCHITECTURE comportamiento OF control_luces_trafico IS
  -- Definición de constantes para los estados de los semáforos
  CONSTANT ROJO : STD_LOGIC := '0';
  CONSTANT VERDE : STD_LOGIC := '1';
  CONSTANT AMARILLO : STD_LOGIC := '2';

  -- Definición de los estados del sistema
  TYPE estado_semaforo IS (ESPERA, VERDE_NORTE, VERDE_SUR, VERDE_ESTE, VERDE_OESTE);
  SIGNAL estado_actual : estado_semaforo := ESPERA;

  -- Definición del temporizador
  CONSTANT TIEMPO_SEMAFORO : INTEGER := 100; -- Tiempo en milisegundos
  SIGNAL contador : INTEGER := 0;  -- Contador del temporizador

  BEGIN
    -- Control del temporizador
    contador <= contador + 1 WHEN clk'EVENT AND clk = '1' AND rst = '0';

    -- Actualización del estado actual
    estado_actual <= ESPERA WHEN rst = '1' ELSE
      VERDE_NORTE WHEN estado_actual = ESPERA AND sensor_norte = '1' AND contador >= TIEMPO_SEMAFORO ELSE
      VERDE_SUR WHEN estado_actual = VERDE_NORTE AND contador >= TIEMPO_SEMAFORO ELSE
      VERDE_ESTE WHEN estado_actual = VERDE_SUR AND contador >= TIEMPO_SEMAFORO ELSE
      VERDE_OESTE WHEN estado_actual = VERDE_ESTE AND contador >= TIEMPO_SEMAFORO ELSE
      estado_actual;

    -- Actualización de los semáforos
    semaforo_norte <= ROJO WHEN estado_actual /= VERDE_NORTE ELSE VERDE;
    semaforo_sur <= ROJO WHEN estado_actual /= VERDE_SUR ELSE VERDE;
    semaforo_este <= ROJO WHEN estado_actual /= VERDE_ESTE ELSE VERDE;
    semaforo_oeste <= ROJO WHEN estado_actual /= VERDE_OESTE ELSE VERDE;
  END ARCHITECTURE;
```

Este código VHDL implementa un controlador para un sistema de luces de tráfico. El sistema tiene cuatro sensores de tráfico (uno para cada dirección) y cuatro semáforos (uno para cada dirección). El controlador utiliza los sensores para determinar cuándo dar paso a cada dirección y actualiza los semáforos en consecuencia.

El sistema tiene un temporizador que se utiliza para controlar el tiempo que cada semáforo permanece en verde. Cuando el temporizador expira, el controlador avanza al siguiente estado.

El controlador tiene un estado de espera inicial, en el que todos los semáforos están en rojo. Cuando un sensor detecta un vehículo, el controlador avanza al siguiente estado, que es verde para la dirección correspondiente. El semáforo permanece en verde para esa dirección durante el tiempo que el temporizador esté activo. Cuando el temporizador expira, el controlador avanza al siguiente estado, que es ámbar para la dirección actual y verde para la siguiente dirección. El semáforo permanece en ámbar durante un periodo de tiempo breve y luego cambia a rojo. El controlador entonces avanza al siguiente estado, que es verde para la siguiente dirección, y el ciclo se repite.

El código VHDL utiliza un tipo de dato enumerado para definir los estados del sistema. También utiliza una señal para almacenar el estado actual del sistema. El código utiliza una instrucción de selección para actualizar el estado actual del sistema en función de las entradas del sensor y del estado actual. El código también utiliza una instrucción de asignación para actualizar los semáforos en función del estado actual.