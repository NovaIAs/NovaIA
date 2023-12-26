Claro! Aqui está um código em Ruby que faz a contagem das letras em uma frase e retorna a quantidade de ocorrências de cada letra, em ordem decrescente:

```ruby
def contagem_letras(frase)
  letras = frase.downcase.gsub(/[^a-zà-ú]/, '').chars
  contagem = letras.each_with_object(Hash.new(0)) { |letra, cont| cont[letra] += 1 }
  contagem = contagem.sort_by { |letra, count| [-count, letra] }

  puts "Contagem das letras na frase:"
  contagem.each do |letra, count|
    puts "#{letra}: #{count}"
  end
end

print "Digite uma frase: "
frase = gets.chomp

contagem_letras(frase)
```

Explicação do código:

1. A função `contagem_letras` recebe uma frase como parâmetro.
2. A variável `letras` armazena todas as letras da frase, convertidas para minúsculas, removendo qualquer caractere não alfabético.
3. A variável `contagem` é inicializada como um objeto Hash vazio, onde as chaves serão as letras e os valores serão as contagens.
4. Utilizamos o método `each_with_object` para iterar sobre cada letra da variável `letras` e atualizar a contagem no objeto Hash `contagem`.
5. Em seguida, ordenamos o objeto `contagem` em ordem decrescente de contagem e ordem alfabética das letras.
6. Finalmente, imprimimos a contagem de cada letra na frase, usando um loop `each`.

Esse código irá fornecer uma contagem das letras presentes em uma frase informada pelo usuário, mostrando a quantidade de ocorrências de cada letra.