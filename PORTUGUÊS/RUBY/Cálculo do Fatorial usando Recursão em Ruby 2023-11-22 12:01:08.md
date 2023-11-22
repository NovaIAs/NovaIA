Claro! Aqui está um código complexo em Ruby, que usa recursão para calcular o fatorial de um número:

```ruby
def fatorial(n)
  if n == 0
    return 1
  else
    return n * fatorial(n-1)
  end
end

puts "Digite um número para calcular o fatorial:"
num = gets.chomp.to_i

resultado = fatorial(num)

puts "O fatorial de #{num} é #{resultado}."
```

Neste código, definimos a função `fatorial` que recebe um número `n` como argumento. Dentro da função, verificamos se `n` é igual a zero. Se for, retornamos 1, pois sabemos que o fatorial de 0 é 1.

Caso contrário, chamamos recursivamente a função `fatorial` passando `n-1` como argumento e multiplicamos o resultado pelo próprio `n`. Isso é feito até que `n` seja reduzido a 0.

Em seguida, pedimos ao usuário para digitar um número e convertemos a entrada para um inteiro usando `gets.chomp.to_i`. Armazenamos o resultado da função `fatorial` para o número digitado na variável `resultado` e, por fim, exibimos o resultado na tela.

Espero que este exemplo te ajude a entender como usar recursão em Ruby para calcular o fatorial de um número. Caso tenha alguma dúvida, estou à disposição para ajudar!