Claro! Aqui está um exemplo de código complexo em VHDL, que consiste em um processador de imagem básico. O código a seguir descreve o funcionamento de um processador de imagem que realiza operações de convolução em uma matriz de pixels.

```vhdl
-- Definição da entidade
entity ImageProcessor is
  generic (
    IMAGE_WIDTH  : integer := 640;
    IMAGE_HEIGHT : integer := 480
  );
  port (
    clk     : in  std_logic;
    reset   : in  std_logic;
    image_in   : in  std_logic_vector(7 downto 0);
    image_out  : out std_logic_vector(7 downto 0)
  );
end entity ImageProcessor;

architecture Behavioral of ImageProcessor is
  type pixel_array is array (natural range <>) of std_logic_vector(7 downto 0);
  
  signal image_buffer : pixel_array(0 to IMAGE_WIDTH-1, 0 to IMAGE_HEIGHT-1);
  signal output_buffer : pixel_array(0 to IMAGE_WIDTH-1, 0 to IMAGE_HEIGHT-1);
  
  -- Definição dos coeficientes do kernel de convolução
  constant kernel : pixel_array(0 to 2, 0 to 2) := (
    (X"01", X"02", X"01"),
    (X"02", X"04", X"02"),
    (X"01", X"02", X"01")
  );
  
begin
  process (clk, reset)
    variable temp : std_logic_vector(7 downto 0);
    variable result : integer range 0 to 255;
  begin
    if reset = '1' then
      -- Redefine todos os pixels da imagem para 0 durante o reset
      for i in 0 to IMAGE_WIDTH-1 loop
        for j in 0 to IMAGE_HEIGHT-1 loop
          image_buffer(i, j) <= (others => '0');
        end loop;
      end loop;
    elsif rising_edge(clk) then
      -- Desloca os pixels na imagem
      for i in 0 to IMAGE_WIDTH-2 loop
        for j in 0 to IMAGE_HEIGHT-1 loop
          image_buffer(i, j) <= image_buffer(i+1, j);
        end loop;
      end loop;
      
      -- Lê o novo pixel de entrada
      temp := image_in;
      image_buffer(IMAGE_WIDTH-1, 0) <= temp;
      
      -- Realiza a convolução dos pixels
      result := 0;
      for i in 0 to 2 loop
        for j in 0 to 2 loop
          result := result + to_integer(unsigned(image_buffer(IMAGE_WIDTH-1-i, j)))*to_integer(unsigned(kernel(i, j)));
        end loop;
      end loop;
      
      -- Limita o resultado entre 0 e 255
      if result < 0 then
        result := 0;
      elsif result > 255 then
        result := 255;
      end if;
      
      -- Armazena o resultado na imagem de saída
      output_buffer(IMAGE_WIDTH-1, 0) <= std_logic_vector(to_unsigned(result, 8));
      
      -- Retorna o pixel de saída
      image_out <= output_buffer(0, 0);
    end if;
  end process;
end architecture Behavioral;
```

Neste código, estamos utilizando o VHDL para implementar um processador de imagem simples. A entidade `ImageProcessor` é definida com uma porta de clock (`clk`), um sinal de reset (`reset`), um sinal de entrada da imagem (`image_in`) e um sinal de saída da imagem (`image_out`).

A arquitetura `Behavioral` descreve a lógica interna do processador de imagem. Ele utiliza uma matriz de pixels (`image_buffer`) para armazenar a imagem de entrada e uma matriz de pixels (`output_buffer`) para armazenar a imagem de saída. A matriz `kernel` define os coeficientes do kernel de convolução.

O código utiliza um processo sensível a borda de subida do clock (`rising_edge(clk)`) para realizar as operações de deslocamento dos pixels na imagem e a convolução dos pixels utilizando o kernel definido. O resultado da convolução é limitado entre 0 e 255 e armazenado na matriz `output_buffer`. Por fim, o sinal de saída `image_out` é atualizado com o pixel de saída correspondente.

Espero que este exemplo seja útil para você!