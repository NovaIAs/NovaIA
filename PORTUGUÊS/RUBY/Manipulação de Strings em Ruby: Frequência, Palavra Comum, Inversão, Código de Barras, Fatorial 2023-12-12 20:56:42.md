Claro! Aqui está um código complexo em Ruby que envolve a manipulação de strings:

```ruby
# Função para calcular a frequência das palavras em uma frase
def calcular_frequencia(frase)
  frequencia = Hash.new(0)
  palavras = frase.downcase.gsub(/[^a-záàâãéèêíïóôõöúçñ\-\'\s]/, '').split(' ')
  
  palavras.each do |palavra|
    frequencia[palavra] += 1
  end
  
  frequencia
end

# Função para encontrar a palavra mais comum em uma frase
def encontrar_palavra_comum(frase)
  frequencia = calcular_frequencia(frase)
  palavra_comum = ''
  max_frequencia = 0
  
  frequencia.each do |palavra, frequencia|
    if frequencia > max_frequencia
      max_frequencia = frequencia
      palavra_comum = palavra
    end
  end
  
  palavra_comum
end

# Função para inverter a ordem das palavras em uma frase
def inverter_ordem_palavras(frase)
  palavras = frase.split(' ')
  palavras.reverse.join(' ')
end

# Função para converter uma frase em código de barras
def converter_para_codigo_de_barras(frase)
  codigo = ''
  
  frase.each_char do |ch|
    codigo += ch.ord.to_s
  end
  
  codigo
end

# Função para calcular o fatorial de um número usando recursão
def calcular_fatorial(numero)
  if numero == 0
    1
  else
    numero * calcular_fatorial(numero - 1)
  end
end

# Exemplo de uso das funções
frase = "Esta é uma frase de exemplo para testar as funções"
puts "Frase original: #{frase}"
puts "Frequência das palavras: #{calcular_frequencia(frase)}"
puts "Palavra mais comum: #{encontrar_palavra_comum(frase)}"
puts "Frase com palavras invertidas: #{inverter_ordem_palavras(frase)}"
puts "Frase convertida para código de barras: #{converter_para_codigo_de_barras(frase)}"
puts "Fatorial de 5: #{calcular_fatorial(5)}"
```

Neste código em Ruby, são definidas várias funções para realizar diferentes tarefas em uma frase. A função `calcular_frequencia` recebe uma frase como entrada e retorna um hash com a frequência de cada palavra. A função `encontrar_palavra_comum` encontra a palavra mais comum na frase com base na sua frequência. A função `inverter_ordem_palavras` inverte a ordem das palavras na frase. A função `converter_para_codigo_de_barras` converte cada caractere da frase para o seu valor ASCII. Por fim, a função `calcular_fatorial` calcula o fatorial de um número usando recursão.

No exemplo de uso das funções, uma frase é definida e as diferentes funções são aplicadas a ela, mostrando os resultados na saída. Este código pode ser usado como base para manipulação de strings em Ruby, permitindo realizar várias operações interessantes em textos.