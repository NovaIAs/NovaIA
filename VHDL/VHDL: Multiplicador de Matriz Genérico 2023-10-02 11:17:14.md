Claro! Aqui está um exemplo de código VHDL complexo que implementa um multiplicador de matriz genérico.

```vhdl
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity MatrixMultiplier is
  generic (
    ROWS_A    : natural := 4;  -- Número de linhas da matriz A
    COLS_A    : natural := 4;  -- Número de colunas da matriz A
    COLS_B    : natural := 4   -- Número de colunas da matriz B
  );
  port (
    clk       : in  std_logic;                           -- Sinal de clock
    reset     : in  std_logic;                           -- Sinal de reset
    enable    : in  std_logic;                           -- Sinal de habilitação
    matrix_a  : in  std_logic_vector(ROWS_A*COLS_A-1 downto 0);  -- Matriz A de entrada
    matrix_b  : in  std_logic_vector(COLS_A*COLS_B-1 downto 0);  -- Matriz B de entrada
    result    : out std_logic_vector(ROWS_A*COLS_B-1 downto 0)   -- Resultado da multiplicação
  );
end entity MatrixMultiplier;

architecture behavioral of MatrixMultiplier is
  type matrix_a_type is array(0 to ROWS_A-1, 0 to COLS_A-1) of unsigned(COLS_A-1 downto 0);
  type matrix_b_type is array(0 to COLS_A-1, 0 to COLS_B-1) of unsigned(COLS_B-1 downto 0);
  type result_type is array(0 to ROWS_A-1, 0 to COLS_B-1) of unsigned(COLS_B-1 downto 0);

  signal matrix_a_internal : matrix_a_type;
  signal matrix_b_internal : matrix_b_type;
  signal result_internal   : result_type;
begin

  -- Conversão dos sinais de entrada para os tipos internos
  process(clk, reset)
  begin
    if reset = '1' then
      matrix_a_internal <= (others => (others => '0'));
      matrix_b_internal <= (others => (others => '0'));
      result_internal   <= (others => (others => '0'));
    elsif rising_edge(clk) then
      if enable = '1' then
        for i in 0 to ROWS_A-1 loop
          for j in 0 to COLS_A-1 loop
            matrix_a_internal(i, j) <= unsigned(matrix_a((i*COLS_A+j)*COLS_A-1 downto (i*COLS_A+j)*COLS_A));
          end loop;
        end loop;

        for i in 0 to COLS_A-1 loop
          for j in 0 to COLS_B-1 loop
            matrix_b_internal(i, j) <= unsigned(matrix_b((i*COLS_B+j)*COLS_B-1 downto (i*COLS_B+j)*COLS_B));
          end loop;
        end loop;
      end if;
    end if;
  end process;

  -- Multiplicação das matrizes
  process(matrix_a_internal, matrix_b_internal)
  begin
    result_internal <= (others => (others => '0'));
    for i in 0 to ROWS_A-1 loop
      for k in 0 to COLS_B-1 loop
        for j in 0 to COLS_A-1 loop
          result_internal(i, k) <= result_internal(i, k) + matrix_a_internal(i, j) * matrix_b_internal(j, k);
        end loop;
      end loop;
    end loop;
  end process;

  -- Conversão do resultado para o tipo de saída
  process(result_internal)
  begin
    for i in 0 to ROWS_A-1 loop
      for j in 0 to COLS_B-1 loop
        result((i*COLS_B+j)*COLS_B-1 downto (i*COLS_B+j)*COLS_B) <= std_logic_vector(result_internal(i, j));
      end loop;
    end loop;
  end process;

end architecture behavioral;
```

Este código implementa um multiplicador de matriz genérico em VHDL. Ele permite definir o número de linhas e colunas para as matrizes A e B através dos parâmetros genéricos ROWS_A, COLS_A e COLS_B. O código converte as entradas da matriz A e B para os tipos internos adequados, realiza a multiplicação das matrizes e converte o resultado de volta para o tipo de saída. O sinal de clock (clk), reset e enable são usados para controlar o processamento dos dados. O resultado final é fornecido através do sinal de saída result.