```vhdl
-- Declaración del módulo complejo en VHDL
módulo complejo es
    -- Definición de los puertos de entrada y salida del módulo
    puerto (
        clk : in std_logic; -- Reloj de sistema
        rst : in std_logic; -- Reinicio de sistema
        a : in integer; -- Entrada real del número complejo
        b : in integer; -- Entrada imaginaria del número complejo
        c : out integer; -- Salida real del número complejo
        d : out integer  -- Salida imaginaria del número complejo
    );
    -- Definición de las variables internas del módulo
    variable r_reg : integer := 0; -- Registro para la parte real
    variable i_reg : integer := 0; -- Registro para la parte imaginaria
    variable cnt : integer := 0; -- Contador para el cálculo de la parte real
    -- Cuerpo del módulo
    proceso (clk) es
        -- Reinicio del sistema
        if rst then
            r_reg <= 0;
            i_reg <= 0;
            cnt <= 0;
        elsif rising_edge(clk) then
            -- Cálculo de la parte real
            if cnt < a then
                r_reg <= r_reg + 1;
                cnt <= cnt + 1;
            else
                cnt <= 0;
            end if;

            -- Cálculo de la parte imaginaria
            if cnt < b then
                i_reg <= i_reg + 1;
                cnt <= cnt + 1;
            else
                cnt <= 0;
            end if;

            -- Salida del número complejo
            c <= r_reg;
            d <= i_reg;
        end if;
    end process;
end módulo;
```

Explicación del código:

* El módulo `complejo` tiene tres puertos de entrada: `a` y `b` (las partes real e imaginaria del número complejo) y `clk` (el reloj de sistema). Tiene dos puertos de salida: `c` y `d` (las partes real e imaginaria del resultado).
* El módulo utiliza una variable interna `r_reg` para almacenar la parte real del número complejo y una variable `i_reg` para almacenar la parte imaginaria. También utiliza una variable `cnt` para llevar la cuenta del número de ciclos de reloj que han transcurrido desde que el módulo comenzó a calcular la parte real del número complejo.
* El proceso `proceso (clk)` se ejecuta en cada ciclo de reloj. Si el sistema se reinicia, las variables `r_reg`, `i_reg` y `cnt` se ponen a cero. De lo contrario, el proceso calcula la parte real y la parte imaginaria del número complejo.
* El cálculo de la parte real se realiza mediante un bucle `for` que se ejecuta `a` veces. En cada iteración del bucle, la variable `r_reg` se incrementa en uno y la variable `cnt` se incrementa en uno. Cuando el bucle termina, la variable `r_reg` contiene la parte real del número complejo.
* El cálculo de la parte imaginaria se realiza mediante un bucle `for` que se ejecuta `b` veces. En cada iteración del bucle, la variable `i_reg` se incrementa en uno y la variable `cnt` se incrementa en uno. Cuando el bucle termina, la variable `i_reg` contiene la parte imaginaria del número complejo.
* Finalmente, las variables `r_reg` y `i_reg` se asignan a las salidas `c` y `d`, respectivamente.