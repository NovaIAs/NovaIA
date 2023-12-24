Claro! Aqui está um exemplo de código em Ruby que realiza a criptografia de uma mensagem usando o algoritmo de Cifra de César. Esse código está em português e possui um tamanho considerável:

```ruby
def criptografar(mensagem, chave)
  mensagem_criptografada = ""
  mensagem.each_char do |caractere|
    if ('a'..'z').include?(caractere)
      novo_caractere = rotacionar(caractere, chave, 'a', 'z')
    elsif ('A'..'Z').include?(caractere)
      novo_caractere = rotacionar(caractere, chave, 'A', 'Z')
    else
      novo_caractere = caractere
    end
    mensagem_criptografada += novo_caractere
  end
  mensagem_criptografada
end

def rotacionar(caractere, chave, inicio, fim)
  indice_atual = caractere.ord - inicio.ord
  indice_rotacionado = (indice_atual + chave) % 26
  novo_caractere = (inicio.ord + indice_rotacionado).chr
  novo_caractere
end

puts "Digite a mensagem a ser criptografada: "
mensagem = gets.chomp

puts "Digite a chave de criptografia (um número inteiro): "
chave = gets.chomp.to_i

mensagem_criptografada = criptografar(mensagem, chave)
puts "Mensagem criptografada: #{mensagem_criptografada}"
```

Neste código, a função `criptografar` recebe uma mensagem e uma chave de criptografia como parâmetros. Em seguida, itera sobre cada caractere da mensagem e, se for uma letra, chama a função `rotacionar` para obter o novo caractere criptografado. Caso contrário, mantém o caractere original. O resultado final é armazenado na variável `mensagem_criptografada` e é exibido ao usuário.

A função `rotacionar` recebe um caractere, uma chave, o caractere inicial e final permitido, e retorna o novo caractere após a criptografia. O algoritmo utiliza o conceito de rotação, onde o caractere é deslocado na tabela ASCII com base na chave fornecida.

No final do código, o usuário é solicitado a digitar a mensagem e a chave de criptografia. Após a execução, a mensagem criptografada é exibida na tela.