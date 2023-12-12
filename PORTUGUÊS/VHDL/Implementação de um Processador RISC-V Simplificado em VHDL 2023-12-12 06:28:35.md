Claro! Aqui está um código em VHDL que implementa um processador RISC-V simplificado. Vou explicar brevemente o que cada parte faz:

```
-- Definição da entidade
entity Processador is
  port (
    clk : in std_logic;
    reset : in std_logic;
    enable : in std_logic;
    instruction : in std_logic_vector(31 downto 0);
    output : out std_logic_vector(31 downto 0)
  );
end Processador;

architecture Behavioral of Processador is
  -- Declaração dos sinais internos
  signal pc : unsigned(31 downto 0);
  signal regfile : std_logic_vector(31 downto 0);
  signal alu_result : std_logic_vector(31 downto 0);
  
  -- Declaração das constantes
  constant OPCODE_LW : std_logic_vector(6 downto 0) := "0000011";
  constant OPCODE_SW : std_logic_vector(6 downto 0) := "0100011";
  constant OPCODE_ADD : std_logic_vector(6 downto 0) := "0110011";
  
begin
  -- Processo principal
  process(clk, reset)
  begin
    if reset = '1' then
      -- Reinicia o processador
      pc <= (others => '0');
      regfile <= (others => '0');
      output <= (others => '0');
    elsif rising_edge(clk) then
      if enable = '1' then
        -- Decodifica a instrução
        case instruction(6 downto 0) is
          when OPCODE_LW =>
            -- Implementação da instrução LW
            alu_result <= regfile(to_integer(unsigned(instruction(19 downto 15)))) + 
                          signed(to_integer(unsigned(instruction(31 downto 20))));
          when OPCODE_SW =>
            -- Implementação da instrução SW
            alu_result <= regfile(to_integer(unsigned(instruction(19 downto 15)))) + 
                          signed(to_integer(unsigned(instruction(31 downto 20))));
          when OPCODE_ADD =>
            -- Implementação da instrução ADD
            alu_result <= regfile(to_integer(unsigned(instruction(19 downto 15)))) + 
                          regfile(to_integer(unsigned(instruction(24 downto 20))));
          -- Outras instruções...
        end case;
        
        -- Atualiza o registrador de destino
        if instruction(6 downto 0) /= OPCODE_SW then
          regfile(to_integer(unsigned(instruction(11 downto 7)))) <= alu_result;
        end if;
        
        -- Atualiza o contador de programa
        pc <= pc + 4;
      end if;
    end if;
  end process;
  
  -- Sinal de saída
  output <= regfile(to_integer(unsigned(instruction(11 downto 7))));
  
end Behavioral;
```

Este código implementa um processador RISC-V simplificado em VHDL. Ele possui uma entidade chamada "Processador" com as portas de entrada e saída necessárias para a comunicação com o mundo exterior. O processador é composto por uma arquitetura chamada "Behavioral". 

Dentro da arquitetura, declaramos alguns sinais internos, como o contador de programa (pc), o registrador de arquivos (regfile) e o resultado da ALU (alu_result). Também declaramos constantes para os códigos de operação das instruções suportadas (LW, SW e ADD).

Em seguida, temos um processo principal que é sensível aos sinais de clock (clk) e reset. Quando o sinal de reset é ativado, o processador é reiniciado e todos os sinais internos são resetados. Caso contrário, na borda de subida do clock, verificamos se o sinal de enable está ativado. Se estiver, decodificamos a instrução e executamos a operação correspondente.

Neste exemplo, implementei apenas as instruções LW, SW e ADD para simplificar. No caso da instrução LW, somamos o valor contido no registrador especificado na instrução com o valor imediato também especificado na instrução. O resultado é armazenado no sinal alu_result. As instruções SW e ADD seguem a mesma lógica, mas não atualizam o registrador de destino.

Por fim, atualizamos o registrador de destino e o contador de programa. O resultado final é armazenado no sinal de saída "output".

Este é apenas um exemplo básico de um processador RISC-V simplificado em VHDL. Existem muitos outros recursos e instruções que podem ser implementados para tornar o processador mais completo e funcional.