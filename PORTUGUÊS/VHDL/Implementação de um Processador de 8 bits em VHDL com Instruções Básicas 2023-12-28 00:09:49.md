Claro! Aqui está um exemplo de código VHDL complexo que implementa um processador simples de 8 bits com instruções básicas. O código contém comentários explicativos para facilitar a compreensão:

```vhdl
-- Definição de pacote para uso de tipos personalizados
package tipos_personalizados is
    type registrador_t is array (natural range <>) of std_logic_vector(7 downto 0);
    type memoria_t is array (natural range <>) of std_logic_vector(7 downto 0);
end package tipos_personalizados;

-- Entidade principal do processador
entity processador_8bits is
    generic (
        TAM_MEMORIA: positive := 256; -- Tamanho da memória em bytes
        TAM_REGISTRADORES: positive := 16 -- Número de registradores disponíveis
    );
    port (
        clk: in std_logic; -- Sinal de clock
        reset: in std_logic; -- Sinal de reset
        entrada: in std_logic_vector(7 downto 0); -- Dados de entrada
        saida: out std_logic_vector(7 downto 0) -- Dados de saída
    );
end processador_8bits;

-- Arquitetura do processador
architecture arq_processador_8bits of processador_8bits is
    -- Declaração dos registradores e sinais internos
    signal registradores: tipos_personalizados.registrador_t(0 to TAM_REGISTRADORES - 1);
    signal memoria: tipos_personalizados.memoria_t(0 to TAM_MEMORIA - 1);
    signal pc: natural range 0 to TAM_MEMORIA - 1;
    signal ir: std_logic_vector(7 downto 0);
    signal opcode: std_logic_vector(3 downto 0);
    signal operando1: std_logic_vector(7 downto 0);
    signal operando2: std_logic_vector(7 downto 0);

begin
    -- Processo principal responsável pela execução das instruções
    process(clk, reset)
    begin
        if reset = '1' then
            -- Reset do processador
            pc <= 0;
            registradores <= (others => (others => '0'));
            memoria <= (others => (others => '0'));
        elsif rising_edge(clk) then
            -- Fetch da próxima instrução
            ir <= memoria(pc);
            opcode <= ir(7 downto 4);
            operando1 <= registradores(to_integer(unsigned(ir(3 downto 0))));
            operando2 <= registradores(to_integer(unsigned(ir(7 downto 4))));

            -- Decodificação e execução das instruções
            case opcode is
                when "0000" => -- Instrução ADD
                    registradores(to_integer(unsigned(ir(7 downto 4)))) <= std_logic_vector(unsigned(operando1) + unsigned(operando2));
                when "0001" => -- Instrução SUB
                    registradores(to_integer(unsigned(ir(7 downto 4)))) <= std_logic_vector(unsigned(operando1) - unsigned(operando2));
                when "0010" => -- Instrução AND
                    registradores(to_integer(unsigned(ir(7 downto 4)))) <= operando1 and operando2;
                when "0011" => -- Instrução OR
                    registradores(to_integer(unsigned(ir(7 downto 4)))) <= operando1 or operando2;
                when "0100" => -- Instrução XOR
                    registradores(to_integer(unsigned(ir(7 downto 4)))) <= operando1 xor operando2;
                when "0101" => -- Instrução LOAD
                    registradores(to_integer(unsigned(ir(7 downto 4)))) <= memoria(to_integer(unsigned(operando2)));
                when "0110" => -- Instrução STORE
                    memoria(to_integer(unsigned(operando2))) <= registradores(to_integer(unsigned(ir(7 downto 4))));
                when "0111" => -- Instrução JUMP
                    pc <= to_integer(unsigned(operando2));
                when "1000" => -- Instrução JUMPZ
                    if operando1 = (others => '0') then
                        pc <= to_integer(unsigned(operando2));
                    end if;
                when others =>
                    null; -- Instrução inválida, não faz nada
            end case;

            -- Incremento do contador de programa
            pc <= pc + 1;
        end if;
    end process;
    
    -- Saída dos dados do registrador 0 no sinal de saída
    saida <= registradores(0);
end arq_processador_8bits;
```

Esse código implementa um processador de 8 bits simples que pode executar instruções básicas como ADD, SUB, AND, OR, XOR, LOAD, STORE, JUMP e JUMPZ. Ele possui uma memória de 256 bytes e 16 registradores disponíveis. O sinal de clock e reset são utilizados para controlar o processador. Os dados de entrada são fornecidos na porta "entrada" e os dados de saída são obtidos na porta "saida". O código também utiliza pacotes para definição de tipos personalizados, facilitando a legibilidade e manutenção do código.