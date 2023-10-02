Claro! Aqui está um código complexo em VHDL que pode ser usado para implementar um multiplicador de matrizes:

```vhdl
entity matrix_multiplier is
  generic (
    N : integer := 4
  );
  port (
    clk : in std_logic;
    reset : in std_logic;
    matrix_a : in std_logic_vector(N*N-1 downto 0);
    matrix_b : in std_logic_vector(N*N-1 downto 0);
    matrix_result : out std_logic_vector(N*N-1 downto 0);
    valid_output : out std_logic
  );
end entity matrix_multiplier;

architecture rtl of matrix_multiplier is

  type matrix_array is array (0 to N-1, 0 to N-1) of std_logic_vector(N-1 downto 0);

  signal a : matrix_array;
  signal b : matrix_array;
  signal result : matrix_array;
  signal row_index : integer range 0 to N-1;
  signal column_index : integer range 0 to N-1;
  signal partial_product : std_logic_vector(2*N-1 downto 0);
  signal sum : std_logic_vector(2*N-1 downto 0);
  signal output_valid : std_logic;
  
begin

  process (clk, reset)
  begin
    if reset = '1' then
      a <= (others => (others => '0'));
      b <= (others => (others => '0'));
      result <= (others => (others => '0'));
      row_index <= 0;
      column_index <= 0;
      sum <= (others => '0');
      output_valid <= '0';
    elsif rising_edge(clk) then
      if row_index = 0 and column_index = 0 then
        a <= matrix_a;
        b <= matrix_b;
        output_valid <= '0';
        
        for i in 0 to N-1 loop
          for j in 0 to N-1 loop
            result(i, j) <= (others => '0');
          end loop;
        end loop;
      end if;
      
      if column_index = N-1 then
        partial_product <= (others => '0');
        for i in 0 to N-1 loop
          partial_product <= std_logic_vector(unsigned(partial_product) + unsigned(a(row_index, i)) * unsigned(b(i, column_index)));
        end loop;
        sum <= std_logic_vector(unsigned(sum) + unsigned(partial_product));
        
        if row_index = N-1 then
          result(row_index, column_index) <= sum(N-1 downto 0);
          sum <= sum(2*N-1 downto N);
          column_index <= column_index + 1;
          output_valid <= '1';
        else
          row_index <= row_index + 1;
        end if;
      else
        partial_product <= (others => '0');
        for i in 0 to N-1 loop
          partial_product <= std_logic_vector(unsigned(partial_product) + unsigned(a(row_index, i)) * unsigned(b(i, column_index)));
        end loop;
        sum <= std_logic_vector(unsigned(sum) + unsigned(partial_product));
        column_index <= column_index + 1;
      end if;
    end if;
  end process;

  matrix_result <= std_logic_vector(result);
  valid_output <= output_valid;

end architecture rtl;
```

Esse código implementa um multiplicador de matrizes NxN em VHDL. Ele utiliza sinais e tipos de dados compostos para armazenar as matrizes de entrada (matrix_a e matrix_b), a matriz resultante (matrix_result) e um sinal de controle (valid_output) que indica quando a saída é válida.

O multiplicador de matrizes usa um processo sensível à borda de subida do clock (clk) e um sinal de reset (reset) para controlar o fluxo de execução. Ele usa sinais de índice (row_index e column_index) para iterar sobre os elementos das matrizes de entrada e calcular o produto parcial e a soma acumulada.

A saída do multiplicador de matrizes é atualizada em cada ciclo de clock e a matriz resultante é armazenada em result. Quando todos os elementos da matriz resultante foram calculados, o sinal de controle output_valid é setado para indicar que a saída é válida.

Esse código é apenas um exemplo e pode ser adaptado para diferentes tamanhos de matriz ou requisitos específicos.