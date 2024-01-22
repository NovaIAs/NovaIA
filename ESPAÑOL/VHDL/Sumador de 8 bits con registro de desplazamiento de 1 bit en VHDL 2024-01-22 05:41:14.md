```vhdl
-- Definición del módulo
módulo top es

    -- Definición de señales
    señal clk : std_logic;
    señal rst : std_logic;
    señal dato_in : std_logic_vector(7 downto 0);
    señal dato_out : std_logic_vector(7 downto 0);

    -- Instanciación de componentes
    componente sumador es
        puerto (
            a : in std_logic_vector(7 downto 0);
            b : in std_logic_vector(7 downto 0);
            suma : out std_logic_vector(8 downto 0);
            acarreo : out std_logic
        );
    end componente;

    componente registro_de_desplazamiento es
        puerto (
            clk : in std_logic;
            rst : in std_logic;
            dato_in : in std_logic;
            dato_out : out std_logic
        );
    end componente;

    -- Conexión de componentes
    sumador_1 : sumador port map (
        a => dato_in,
        b => dato_in,
        suma => dato_out,
        acarreo => acarreo
    );

    registro_de_desplazamiento_1 : registro_de_desplazamiento port map (
        clk => clk,
        rst => rst,
        dato_in => acarreo,
        dato_out => dato_in
    );

-- Fin del módulo
end módulo;
```

**Explicación del código:**

* El código VHDL define un módulo llamado `top` que implementa un circuito sumador de 8 bits con un registro de desplazamiento de 1 bit.
* El módulo `top` tiene tres señales de entrada: `clk` (reloj), `rst` (reset) y `dato_in` (dato de entrada).
* El módulo `top` tiene una señal de salida: `dato_out` (dato de salida).
* El módulo `top` instancia dos componentes: un sumador de 8 bits y un registro de desplazamiento de 1 bit.
* El sumador de 8 bits suma los datos de entrada `dato_in` y `dato_in` y almacena el resultado en la señal `dato_out`.
* El registro de desplazamiento de 1 bit almacena el acarreo del sumador de 8 bits en la señal `dato_in`.
* Cuando el reloj `clk` está activo y el reset `rst` está desactivado, el registro de desplazamiento de 1 bit desplaza el acarreo del sumador de 8 bits a la izquierda y almacena el resultado en la señal `dato_in`.
* Cuando el reloj `clk` está activo y el reset `rst` está activado, el registro de desplazamiento de 1 bit se resetea y almacena un 0 en la señal `dato_in`.