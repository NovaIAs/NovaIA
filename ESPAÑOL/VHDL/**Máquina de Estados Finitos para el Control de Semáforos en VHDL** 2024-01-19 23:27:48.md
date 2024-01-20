```vhdl
-- Código VHDL para implementar una Máquina de Estados Finitos (MEF) que controla un sistema de semáforos.
-- El sistema de semáforos tiene dos semáforos: uno para el tráfico norte-sur y otro para el tráfico este-oeste.

-- Definición de los estados de la MEF.
type estado is (rojo_norte_sur, rojo_este_oeste, verde_norte_sur, verde_este_oeste);

-- Definición de las entradas y salidas de la MEF.
signal entrada_norte_sur : std_logic; -- Señal de entrada para el tráfico norte-sur.
signal entrada_este_oeste : std_logic; -- Señal de entrada para el tráfico este-oeste.
signal salida_rojo_norte_sur : std_logic; -- Señal de salida para el semáforo rojo norte-sur.
signal salida_amarillo_norte_sur : std_logic; -- Señal de salida para el semáforo amarillo norte-sur.
signal salida_verde_norte_sur : std_logic; -- Señal de salida para el semáforo verde norte-sur.
signal salida_rojo_este_oeste : std_logic; -- Señal de salida para el semáforo rojo este-oeste.
signal salida_amarillo_este_oeste : std_logic; -- Señal de salida para el semáforo amarillo este-oeste.
signal salida_verde_este_oeste : std_logic; -- Señal de salida para el semáforo verde este-oeste.

-- Definición del proceso de la MEF.
process
begin
  -- Estado actual de la MEF.
  variable estado_actual : estado := rojo_norte_sur;

  -- Bucle infinito que implementa el comportamiento de la MEF.
  loop
    -- Actualización del estado actual de la MEF en función de las entradas y el estado actual.
    if estado_actual = rojo_norte_sur then
      if entrada_norte_sur = '1' then
        estado_actual := verde_norte_sur;
      elsif entrada_este_oeste = '1' then
        estado_actual := rojo_este_oeste;
      end if;
    elsif estado_actual = verde_norte_sur then
      estado_actual := rojo_norte_sur;
    elsif estado_actual = rojo_este_oeste then
      if entrada_este_oeste = '1' then
        estado_actual := verde_este_oeste;
      elsif entrada_norte_sur = '1' then
        estado_actual := rojo_norte_sur;
      end if;
    elsif estado_actual = verde_este_oeste then
      estado_actual := rojo_este_oeste;
    end if;

    -- Actualización de las salidas de la MEF en función del estado actual.
    case estado_actual is
      when rojo_norte_sur =>
        salida_rojo_norte_sur <= '1';
        salida_amarillo_norte_sur <= '0';
        salida_verde_norte_sur <= '0';
        salida_rojo_este_oeste <= '0';
        salida_amarillo_este_oeste <= '1';
        salida_verde_este_oeste <= '0';
      when verde_norte_sur =>
        salida_rojo_norte_sur <= '0';
        salida_amarillo_norte_sur <= '0';
        salida_verde_norte_sur <= '1';
        salida_rojo_este_oeste <= '1';
        salida_amarillo_este_oeste <= '0';
        salida_verde_este_oeste <= '0';
      when rojo_este_oeste =>
        salida_rojo_norte_sur <= '0';
        salida_amarillo_norte_sur <= '1';
        salida_verde_norte_sur <= '0';
        salida_rojo_este_oeste <= '1';
        salida_amarillo_este_oeste <= '0';
        salida_verde_este_oeste <= '0';
      when verde_este_oeste =>
        salida_rojo_norte_sur <= '1';
        salida_amarillo_norte_sur <= '0';
        salida_verde_norte_sur <= '0';
        salida_rojo_este_oeste <= '0';
        salida_amarillo_este_oeste <= '0';
        salida_verde_este_oeste <= '1';
    end case;

    -- Retardo de un ciclo de reloj.
    wait for 1 ns;
  end loop;
end process;

-- Fin del código VHDL.
```

Explicación del código:

* El código VHDL define una MEF que controla un sistema de semáforos con dos semáforos: uno para el tráfico norte-sur y otro para el tráfico este-oeste.
* La MEF tiene cuatro estados: rojo_norte_sur, rojo_este_oeste, verde_norte_sur y verde_este_oeste.
* La MEF tiene dos entradas: entrada_norte_sur y entrada_este_oeste. Estas entradas indican si hay tráfico en la dirección norte-sur o este-oeste, respectivamente.
* La MEF tiene seis salidas: salida_rojo_norte_sur, salida_amarillo_norte_sur, salida_verde_norte_sur, salida_rojo_este_oeste, salida_amarillo_este_oeste y salida_verde_este_oeste. Estas salidas controlan los semáforos norte-sur y este-oeste.
* El proceso de la MEF es un bucle infinito que se ejecuta continuamente.
* En cada iteración del bucle, la MEF actualiza su estado actual en función de las entradas y el estado actual.
* La MEF también actualiza las salidas en función del estado actual.
* El proceso de la MEF se repite continuamente, controlando así el sistema de semáforos.