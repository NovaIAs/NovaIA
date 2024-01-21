```vhdl
-- Declaración de la entidad
entidad modulo_complejo es
    -- Puertos de entrada
    puerto (
        clk : in std_logic;
        rst : in std_logic;
        dato_entrada : in std_logic_vector(7 downto 0);
    );

    -- Puertos de salida
    puerto (
        dato_salida : out std_logic_vector(7 downto 0);
        indicador_paridad : out std_logic
    );
end modulo_complejo;

-- Declaración de la arquitectura
arquitectura comportamiento of modulo_complejo is
    -- Declaración de señales internas
    signal dato_temporal : std_logic_vector(7 downto 0);
    signal paridad : std_logic;

    -- Proceso para el cálculo de la paridad
    proceso (clk, rst)
    begin
        if rst = '1' then
            paridad <= '0';
        elsif rising_edge(clk) then
            paridad <= not paridad xor dato_temporal(0) xor dato_temporal(1) xor dato_temporal(2) xor dato_temporal(3) xor dato_temporal(4) xor dato_temporal(5) xor dato_temporal(6) xor dato_temporal(7);
        end if;
    end proceso;

    -- Proceso para el registro de datos
    proceso (clk, rst)
    begin
        if rst = '1' then
            dato_temporal <= (others => '0');
        elsif rising_edge(clk) then
            dato_temporal <= dato_entrada;
        end if;
    end proceso;

    -- Asignación de las salidas
    dato_salida <= dato_temporal;
    indicador_paridad <= paridad;
end comportamiento;

```

**Explicación del código:**

* **Entidad:** La entidad define la interfaz del módulo, especificando sus puertos de entrada y salida.
* **Arquitectura:** La arquitectura define la implementación del módulo, especificando el comportamiento de los procesos internos y la asignación de las salidas.
* **Proceso para el cálculo de la paridad:** El proceso calcula la paridad de los datos de entrada, utilizando un XOR para combinar los bits de datos y un flip-flop para almacenar el resultado.
* **Proceso para el registro de datos:** El proceso registra los datos de entrada en un registro de desplazamiento, utilizando un flip-flop para almacenar cada bit de datos.
* **Asignación de las salidas:** Las salidas del módulo se asignan a las señales internas del módulo utilizando el operador <=.

Este código implementa un módulo que calcula la paridad de los datos de entrada y los registra en un registro de desplazamiento. El módulo tiene dos puertos de entrada, `clk` y `rst`, y dos puertos de salida, `dato_salida` e `indicador_paridad`. El puerto `clk` es la señal de reloj del módulo, el puerto `rst` es la señal de reset del módulo, el puerto `dato_entrada` es el puerto de entrada de datos y el puerto `dato_salida` es el puerto de salida de datos. El puerto `indicador_paridad` es el puerto de salida del indicador de paridad.

El módulo funciona de la siguiente manera:

* Cuando el módulo se enciende por primera vez, el registro de desplazamiento se inicializa con todos los bits a 0.
* Cuando la señal de reloj del módulo sube, el módulo calcula la paridad de los datos de entrada y la almacena en el indicador de paridad.
* También registra los datos de entrada en el registro de desplazamiento.
* Los datos de salida del módulo son los datos registrados en el registro de desplazamiento.