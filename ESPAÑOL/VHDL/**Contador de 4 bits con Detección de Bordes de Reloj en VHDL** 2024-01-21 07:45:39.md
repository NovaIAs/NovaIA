```vhdl
-- Código VHDL para un contador de 4 bits con presencia de borde de reloj

-- Declaración de la entidad
entidad contador4bits_pbreloj es
    -- Señales de entrada
    puerto(
        clk : in std_logic; -- Reloj
        reset : in std_logic; -- Reinicio
        en : in std_logic; -- Habilitar
        up_down : in std_logic; -- Dirección de conteo (arriba/abajo)
    );
    -- Señal de salida
    puerto(
        q : out std_logic_vector(3 downto 0) -- Salida del contador
    );
end entidad;

-- Arquitectura del contador
arquitectura comportamiento of contador4bits_pbreloj is
    -- Registro de salida
    señal q_int : std_logic_vector(3 downto 0) := (others => '0');

    -- Proceso de conteo
    proceso(clk, reset) is
        begin
            if reset = '1' then
                q_int <= (others => '0');
            elsif rising_edge(clk) then
                if en = '1' then
                    if up_down = '1' then
                        q_int <= q_int + 1;
                    else
                        q_int <= q_int - 1;
                    end if;
                end if;
            end if;
        end proceso;

    -- Asignación de la salida
    q <= q_int;
end arquitectura comportamiento;
```

Explicación del código:

* **Entidad**: La entidad define las señales de entrada y salida del contador. Las señales de entrada son: clk (reloj), reset (reinicio), en (habilitar) y up_down (dirección de conteo). La señal de salida es q (salida del contador).
* **Arquitectura**: La arquitectura define el comportamiento del contador. El contador utiliza un registro de salida de 4 bits (q_int) para almacenar el valor del contador. El proceso de conteo se ejecuta en el flanco de subida del reloj y se activa cuando la señal en es alta. El contador cuenta hacia arriba o hacia abajo dependiendo del valor de la señal up_down.
* **Proceso de conteo**: El proceso de conteo es un proceso síncrono que se ejecuta en el flanco de subida del reloj. Si la señal reset es alta, el contador se reinicia a cero. Si la señal en es alta, el contador cuenta hacia arriba o hacia abajo dependiendo del valor de la señal up_down. Si la señal up_down es alta, el contador cuenta hacia arriba. Si la señal up_down es baja, el contador cuenta hacia abajo.
* **Asignación de la salida**: La señal de salida q se asigna al valor del registro de salida q_int.