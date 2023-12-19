Claro! Vou lhe fornecer um código complexo em Ruby que lida com manipulação de strings e estruturas de controle. O código a seguir é um exemplo de um programa que verifica se uma frase é um palíndromo, ou seja, se pode ser lida da mesma forma tanto da esquerda para a direita quanto da direita para a esquerda. Ele também conta o número de ocorrências de cada palavra na frase.

```ruby
def verificar_palindromo(frase)
  frase = frase.downcase.gsub(/\s+/, "") # Remove espaços e converte para minúsculas
  reverso = frase.reverse

  if frase == reverso
    puts "A frase é um palíndromo!"
  else
    puts "A frase não é um palíndromo."
  end
end

def contar_ocorrencias_palavras(frase)
  palavras = frase.downcase.split(/\W+/) # Divide a frase em palavras separadas por caracteres não alfanuméricos
  ocorrencias = Hash.new(0)

  palavras.each do |palavra|
    ocorrencias[palavra] += 1
  end

  puts "Ocorrências de palavras:"
  ocorrencias.each do |palavra, ocorrencia|
    puts "#{palavra}: #{ocorrencia}"
  end
end

print "Digite uma frase: "
frase = gets.chomp

verificar_palindromo(frase)
contar_ocorrencias_palavras(frase)
```

Explicação do código:

1. A função `verificar_palindromo` recebe uma frase como parâmetro. Primeiro, a frase é convertida para minúsculas e todos os espaços são removidos usando o método `downcase.gsub(/\s+/, "")`. Em seguida, a variável `reverso` é atribuída à frase original invertida.
2. É feita uma comparação entre a frase original e a frase invertida. Se elas forem iguais, significa que a frase é um palíndromo, caso contrário não é.
3. A função `contar_ocorrencias_palavras` recebe uma frase como parâmetro. Primeiro, a frase é convertida para minúsculas e dividida em palavras separadas por caracteres não alfanuméricos usando o método `downcase.split(/\W+/)`.
4. É criado um hash vazio chamado `ocorrencias` para armazenar as palavras e suas respectivas ocorrências.
5. Para cada palavra na lista de palavras, é incrementado o valor do hash `ocorrencias` utilizando a palavra como chave.
6. Por fim, é exibido o número de ocorrências de cada palavra no hash `ocorrencias`.

Ao executar o código, ele solicitará que você digite uma frase. Em seguida, verificará se a frase é um palíndromo e exibirá o resultado. Em seguida, mostrará o número de ocorrências de cada palavra na frase.