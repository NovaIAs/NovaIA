Claro! Aqui está um código complexo em VHDL que realiza o processamento de um filtro digital FIR (Finite Impulse Response). O código está em português e também inclui explicações detalhadas para melhor entendimento:

```vhdl
-- Definição da entidade do filtro FIR
entity filtro_fir is
  generic (
    N : integer := 8;  -- Tamanho do filtro (número de coeficientes)
    M : integer := 8   -- Tamanho da entrada/saída (número de bits)
  );
  port (
    clk : in std_logic;                   -- Sinal de clock
    reset : in std_logic;                 -- Sinal de reset
    entrada : in std_logic_vector(M-1 downto 0);  -- Sinal de entrada
    saida : out std_logic_vector(M-1 downto 0)   -- Sinal de saída
  );
end filtro_fir;

architecture rtl of filtro_fir is
  type c_array is array (0 to N-1) of std_logic_vector(M-1 downto 0);  -- Tipo para os coeficientes
  signal coeficientes : c_array;  -- Sinal para armazenar os coeficientes
  signal buffer : std_logic_vector(N-1 downto 0);  -- Sinal para armazenar o buffer dos dados de entrada
  
begin
  
  -- Processo para inicialização dos coeficientes
  processo_inicializacao : process (reset)
  begin
    if reset = '1' then
      -- Inicializa os coeficientes com valores pré-definidos
      coeficientes <= (
        "00000001",
        "00000010",
        "00000011",
        "00000100",
        "00000101",
        "00000110",
        "00000111",
        "00001000"
      );
    end if;
  end process processo_inicializacao;

  -- Processo principal para o processamento do filtro FIR
  processo_fir : process (clk)
    variable soma : std_logic_vector(M-1 downto 0);  -- Variável para armazenar a soma dos produtos
  begin
    if rising_edge(clk) then
      if reset = '1' then
        -- Reinicia o buffer e a saída em caso de reset
        buffer <= (others => '0');
        saida <= (others => '0');
      else
        -- Desloca os valores no buffer
        buffer(N-2 downto 0) <= buffer(N-1 downto 1);
        buffer(N-1) <= entrada;
        
        -- Realiza a soma dos produtos
        soma := (others => '0');
        for i in 0 to N-1 loop
          soma := soma + buffer(i) * coeficientes(i);
        end loop;
        
        -- Atribui o resultado à saída
        saida <= soma;
      end if;
    end if;
  end process processo_fir;

end rtl;
```

Este código VHDL implementa um filtro digital FIR de tamanho configurável, com coeficientes predefinidos. O filtro usa um buffer circular para armazenar as amostras de entrada e realiza a multiplicação dos coeficientes pelos valores do buffer para realizar a filtragem.

A entidade `filtro_fir` possui uma entrada de clock (`clk`), um sinal de reset (`reset`), um sinal de entrada (`entrada`) e um sinal de saída (`saida`). Os parâmetros genéricos `N` e `M` permitem configurar o tamanho do filtro e o número de bits das amostras, respectivamente.

A arquitetura `rtl` contém dois processos essenciais: `processo_inicializacao` e `processo_fir`. 

O primeiro processo, `processo_inicializacao`, é acionado pelo sinal de reset e é responsável por inicializar os coeficientes do filtro com valores pré-definidos. Os coeficientes estão armazenados em um sinal de vetor `coeficientes`.

O segundo processo, `processo_fir`, é acionado pelo sinal de clock e realiza o processamento do filtro FIR. Dentro deste processo, os valores de entrada são armazenados no buffer circular `buffer` e os coeficientes são multiplicados pelos valores correspondentes no buffer usando uma estrutura de repetição. O resultado da filtragem é então atribuído ao sinal de saída `saida`.

Espero que esse código atenda às suas necessidades! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição para ajudar.