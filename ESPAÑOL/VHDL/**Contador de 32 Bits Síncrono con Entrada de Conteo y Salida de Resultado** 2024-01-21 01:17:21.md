```vhdl
-- Módulo para implementar un contador de 32 bits con entrada de conteo y salida de resultado
entity Contador32Bits is
    port (
        clk : in std_logic; -- Reloj de entrada
        rst : in std_logic; -- Señal de reset asíncrono activo bajo
        cnt_en : in std_logic; -- Señal de habilitación de conteo
        cnt_in : in std_logic; -- Entrada de conteo
        cnt_out : out std_logic_vector(31 downto 0) -- Salida de resultado del contador
    );
end entity;

architecture Behavioral of Contador32Bits is
    signal cnt_reg : std_logic_vector(31 downto 0) := (others => '0'); -- Registro del contador

begin
    -- Proceso para actualizar el registro del contador en cada ciclo de reloj
    process(clk, rst)
    begin
        if rst = '0' then -- Si la señal de reset está activa
            cnt_reg <= (others => '0'); -- Restablecer el registro del contador a cero
        elsif clk'event and clk = '1' and cnt_en = '1' then -- Si se recibe un flanco positivo de reloj y la señal de habilitación de conteo está activa
            cnt_reg <= cnt_reg + cnt_in; -- Incrementar el registro del contador en el valor de entrada de conteo
        end if;
    end process;

    -- Asignar la salida del contador al registro
    cnt_out <= cnt_reg;
end architecture;
```

Explicación del código:

* El módulo `Contador32Bits` implementa un contador de 32 bits con entrada de conteo y salida de resultado.
* El módulo tiene cuatro puertos: `clk` (reloj de entrada), `rst` (señal de reset asíncrono activo bajo), `cnt_en` (señal de habilitación de conteo) y `cnt_out` (salida de resultado del contador).
* El registro del contador `cnt_reg` se declara como una señal de tipo `std_logic_vector` de 32 bits y se inicializa con todos los bits a cero.
* El proceso `process(clk, rst)` se ejecuta en cada ciclo de reloj y actualiza el registro del contador.
* Si la señal de reset `rst` está activa, se restablece el registro del contador a cero.
* Si se recibe un flanco positivo de reloj y la señal de habilitación de conteo `cnt_en` está activa, se incrementa el registro del contador en el valor de la entrada de conteo `cnt_in`.
* La salida del contador `cnt_out` se asigna al registro del contador `cnt_reg`.