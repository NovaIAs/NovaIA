```vhdl
-- Librerías necesarias
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

-- Constantes
constant N: integer := 16;  -- Tamaño del registro
constant M: integer := 8;   -- Tamaño de la memoria

-- Definición de tipos
type Registro is array(0 to N-1) of STD_LOGIC;
type Memoria is array(0 to M-1) of Registro;

-- Entidad del registro de desplazamiento
entity Registro_Desplazamiento is
    port(
        clk: in STD_LOGIC;
        rst: in STD_LOGIC;
        entrada: in STD_LOGIC;
        salida: out Registro
    );
end Registro_Desplazamiento;

-- Arquitectura del registro de desplazamiento
architecture Registro_Desplazamiento_Arq of Registro_Desplazamiento is
    signal registro: Registro := (others => '0');  -- Registro interno
begin
    -- Proceso para actualizar el registro
    process(clk, rst)
    begin
        if rst = '1' then
            registro <= (others => '0');  -- Reiniciar registro
        elsif rising_edge(clk) then
            registro(0) <= entrada;  -- Entrada en el primer bit
            for i in N-1 downto 1 loop
                registro(i) <= registro(i-1);  -- Desplazar el registro
            end loop;
        end if;
    end process;

    -- Asignar la salida
    salida <= registro;
end Registro_Desplazamiento_Arq;

-- Entidad de la memoria
entity Memoria_Registros is
    port(
        clk: in STD_LOGIC;
        rst: in STD_LOGIC;
        escritura: in STD_LOGIC;
        dirección: in STD_LOGIC_VECTOR(M-1 downto 0);
        entrada: in Registro;
        salida: out Registro
    );
end Memoria_Registros;

-- Arquitectura de la memoria
architecture Memoria_Registros_Arq of Memoria_Registros is
    signal memoria: Memoria := (others => (others => '0'));  -- Memoria interna

begin
    -- Proceso para escribir en la memoria
    process(clk, rst, escritura)
    begin
        if rst = '1' then
            memoria <= (others => (others => '0'));  -- Reiniciar memoria
        elsif rising_edge(clk) and escritura = '1' then
            memoria(dirección) <= entrada;  -- Escribir en la dirección
        end if;
    end process;

    -- Asignar la salida
    salida <= memoria(dirección);
end Memoria_Registros_Arq;

-- Entidad principal
entity Procesador is
    port(
        clk: in STD_LOGIC;
        rst: in STD_LOGIC
    );
end Procesador;

-- Arquitectura principal
architecture Procesador_Arq of Procesador is
    signal registro: Registro;  -- Registro interno
    signal memoria: Memoria;  -- Memoria interna
begin
    -- Instanciar el registro de desplazamiento
    Registro_Desplazamiento: Registro_Desplazamiento port map(
        clk => clk,
        rst => rst,
        entrada => '1',  -- Entrada fija a 1 para este ejemplo
        salida => registro
    );

    -- Instanciar la memoria
    Memoria_Registros: Memoria_Registros port map(
        clk => clk,
        rst => rst,
        escritura => registro(N-1),  -- Bit más significativo como señal de escritura
        dirección => registro(N-2 downto N-M),  -- Bits de dirección
        entrada => registro,
        salida => memoria(0)  -- Salida de la dirección 0
    );

    -- Imprimir el contenido de la memoria en la consola
    process(clk)
    begin
        if rising_edge(clk) then
            for i in 0 to M-1 loop
                put_line(to_string(memoria(i)));
            end loop;
        end if;
    end process;
end Procesador_Arq;
```

**Explicación:**

1. **Entidades y Arquitecturas:** El código se divide en entidades y arquitecturas. Las entidades definen las interfaces de los componentes, mientras que las arquitecturas contienen la lógica interna.
2. **Registro de Desplazamiento:** El registro de desplazamiento es un componente que almacena y desplaza un número de bits en una dirección. En este caso, se utiliza para generar una secuencia de bits que se escribe en la memoria.
3. **Memoria de Registros:** La memoria de registros es un componente que almacena un conjunto de registros. En este caso, se utiliza para almacenar los contenidos del registro de desplazamiento.
4. **Procesador:** El procesador es el componente principal del sistema. Instancia el registro de desplazamiento y la memoria de registros, y conecta las señales entre ellos. También imprime el contenido de la memoria en la consola.

Este código implementa un procesador simple que genera una secuencia de bits usando un registro de desplazamiento y los almacena en una memoria de registros. El contenido de la memoria se imprime en la consola para su visualización.