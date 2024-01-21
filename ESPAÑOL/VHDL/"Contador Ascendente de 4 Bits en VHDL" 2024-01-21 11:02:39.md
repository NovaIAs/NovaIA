```vhdl
-- Código VHDL para un contador ascendente de 4 bits

-- Definición de la entidad del contador
entidad Contador_Ascendente_4_Bits es
  puerto (
    Reloj : in std_logic;
    Reset : in std_logic;
    Conteo : out std_logic_vector(3 downto 0)
  );
end entidad Contador_Ascendente_4_Bits;

-- Definición de la arquitectura del contador
arquitectura Estructural of Contador_Ascendente_4_Bits is
  signal Contador_Interno : std_logic_vector(3 downto 0) := "0000";

begin
  -- Proceso para incrementar el contador en cada flanco de subida del reloj
  proceso (Reloj)
  begin
    if Reloj'event and Reloj = '1' then
      if Reset = '1' then
        Contador_Interno <= "0000";
      else
        Contador_Interno <= Contador_Interno + 1;
      end if;
    end if;
  end proceso;

  -- Asignación del valor del contador a la salida
  Conteo <= Contador_Interno;
end arquitectura Estructural;
```

Este código implementa un contador ascendente de 4 bits en VHDL. El contador cuenta desde 0 hasta 15, y cuando alcanza el valor 15, se reinicia a 0. El contador tiene dos entradas, Reloj y Reset, y una salida, Conteo. La entrada Reloj es el reloj del sistema, que controla la velocidad a la que cuenta el contador. La entrada Reset es una señal de reinicio, que cuando está activa, reinicia el contador a 0. La salida Conteo es un vector de 4 bits que contiene el valor actual del contador.

El código está escrito en VHDL, un lenguaje de descripción de hardware que se utiliza para diseñar circuitos integrados. VHDL es un lenguaje textual que describe la estructura y el comportamiento de un circuito integrado. El código se divide en dos partes: la definición de la entidad y la definición de la arquitectura.

La definición de la entidad describe la interfaz externa del contador. Define los puertos de entrada y salida del contador, y los tipos de datos de las señales que se pasan a través de los puertos. En este caso, el contador tiene dos entradas, Reloj y Reset, y una salida, Conteo. La entrada Reloj es una señal de tipo std_logic, que puede tomar los valores '0' o '1'. La entrada Reset es también una señal de tipo std_logic. La salida Conteo es un vector de 4 bits de tipo std_logic_vector.

La definición de la arquitectura describe el comportamiento interno del contador. Define los procesos y las señales que se utilizan para implementar el contador. En este caso, el contador se implementa utilizando un proceso. El proceso se ejecuta cada vez que cambia la señal Reloj. Si la señal Reloj cambia a '1', el proceso comprueba el valor de la señal Reset. Si la señal Reset está activa, el contador se reinicia a 0. De lo contrario, el contador se incrementa en 1. El valor actual del contador se asigna a la salida Conteo.

El código está escrito en un estilo estructural. Esto significa que el código se organiza en una jerarquía de bloques, donde cada bloque representa un componente del contador. En este caso, el contador se divide en dos bloques: el bloque del contador y el bloque del proceso. El bloque del contador contiene las señales y los procesos que se utilizan para implementar el contador. El bloque del proceso contiene el proceso que se ejecuta cada vez que cambia la señal Reloj.