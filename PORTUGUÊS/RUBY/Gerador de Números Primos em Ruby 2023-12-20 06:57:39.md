Claro! Aqui está um código em Ruby que cria uma classe chamada "GeradorDeNumerosPrimos" que implementa um algoritmo para encontrar todos os números primos em um determinado intervalo.

```ruby
class GeradorDeNumerosPrimos
  def initialize(inicio, fim)
    @inicio = inicio
    @fim = fim
  end
  
  def gerar_primos
    primos = []

    (@inicio..@fim).each do |numero|
      proximo = true
      (2..Math.sqrt(numero)).each do |divisor|
        if (numero % divisor).zero?
          proximo = false
          break
        end
      end

      primos << numero if proximo && numero > 1
    end

    primos
  end
end

# Exemplo de uso
gerador = GeradorDeNumerosPrimos.new(1, 100)
primos = gerador.gerar_primos
puts "Números primos entre 1 e 100: #{primos.join(', ')}"
```

Explicação do código:
1. A classe `GeradorDeNumerosPrimos` é criada para encapsular a lógica de geração de números primos.
2. O construtor `initialize` recebe dois parâmetros: `inicio` e `fim`, que definem o intervalo em que os números primos serão gerados.
3. O método `gerar_primos` é responsável por encontrar e retornar todos os números primos dentro do intervalo especificado.
4. A variável `primos` é inicializada como um array vazio para armazenar os números primos encontrados.
5. Um loop `each` é usado para iterar por todos os números no intervalo de `@inicio` até `@fim`.
6. Para cada número, outro loop `each` é usado para verificar se ele é divisível por algum número entre 2 e sua raiz quadrada (`Math.sqrt(numero)`).
7. Se o número for divisível por algum desses valores, a variável `proximo` é definida como false e o loop é encerrado.
8. Caso contrário, se o número não for divisível por nenhum desses valores e for maior que 1, ele é considerado um número primo e adicionado ao array `primos`.
9. Após o término do loop, o array `primos` contém todos os números primos encontrados no intervalo.
10. Por fim, um exemplo de uso é mostrado, criando uma instância da classe `GeradorDeNumerosPrimos` para encontrar os números primos entre 1 e 100 e exibindo-os na tela.

Espero que este código atenda às suas expectativas!