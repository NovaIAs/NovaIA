Claro! Aqui está um código complexo em Ruby para criar um algoritmo que verifica se um número é um palíndromo:

```ruby
def eh_palindromo(numero)
  # Convertendo o número para uma string
  numero_string = numero.to_s
  
  # Verificando se o número é um palíndromo
  if numero_string == numero_string.reverse
    return true
  else
    return false
  end
end

def verificar_palindromos(numeros)
  palindromos = []
  
  # Verificando se cada número é um palíndromo e adicionando-o à lista
  numeros.each do |numero|
    if eh_palindromo(numero)
      palindromos << numero
    end
  end
  
  return palindromos
end

# Exemplo de uso
numeros = [12321, 45654, 78987, 12345, 67876]
palindromos = verificar_palindromos(numeros)

puts "Números palíndromos encontrados: #{palindromos}"
```

Explicação do código:

1. A função `eh_palindromo` verifica se um número é um palíndromo. Ela recebe um número como argumento e converte-o para uma string, usando o método `to_s`. Em seguida, ela compara essa string com sua versão invertida, usando o método `reverse`. Se as duas strings forem iguais, o número é um palíndromo e a função retorna `true`, caso contrário, retorna `false`.

2. A função `verificar_palindromos` recebe uma lista de números como argumento. Ela cria uma lista vazia `palindromos` para armazenar os números palíndromos encontrados.

3. A função percorre cada número da lista usando o método `each`. Para cada número, ele chama a função `eh_palindromo` para verificar se é um palíndromo. Se for, o número é adicionado à lista `palindromos` usando o operador `<<`.

4. Por fim, a função retorna a lista `palindromos`.

5. No exemplo de uso, criamos uma lista de números `numeros` e chamamos a função `verificar_palindromos` passando essa lista como argumento. A função retorna a lista de números palíndromos encontrados e a exibimos na tela usando o método `puts`.