Claro! Aqui está um exemplo de um código complexo em Ruby que cria uma calculadora avançada com funções personalizadas. Vou explicar cada parte para que você possa entender:

```ruby
class CalculadoraAvancada
  def initialize
    @memoria = 0
  end
  
  def somar(a, b)
    resultado = a + b
    salvar_memoria(resultado)
    return resultado
  end
  
  def subtrair(a, b)
    resultado = a - b
    salvar_memoria(resultado)
    return resultado
  end
  
  def multiplicar(a, b)
    resultado = a * b
    salvar_memoria(resultado)
    return resultado
  end
  
  def dividir(a, b)
    resultado = a / b
    salvar_memoria(resultado)
    return resultado
  end
  
  def exponencial(a, b)
    resultado = a ** b
    salvar_memoria(resultado)
    return resultado
  end
  
  def raiz_quadrada(a)
    resultado = Math.sqrt(a)
    salvar_memoria(resultado)
    return resultado
  end
  
  def memoria
    return @memoria
  end
  
  private
  
  def salvar_memoria(resultado)
    @memoria = resultado
  end
end

# Exemplo de uso:
calculadora = CalculadoraAvancada.new
puts calculadora.somar(2, 3) # Saída: 5
puts calculadora.subtrair(5, 2) # Saída: 3
puts calculadora.multiplicar(4, 5) # Saída: 20
puts calculadora.dividir(10, 2) # Saída: 5
puts calculadora.exponencial(2, 3) # Saída: 8
puts calculadora.raiz_quadrada(9) # Saída: 3
puts calculadora.memoria # Saída: 3
```

Neste exemplo, criamos uma classe chamada `CalculadoraAvancada` que possui seis métodos para realizar operações matemáticas: `somar`, `subtrair`, `multiplicar`, `dividir`, `exponencial` e `raiz_quadrada`. Além disso, a classe também possui um método `memoria` que mostra o valor atual armazenado na memória.

A função `initialize` é chamada quando uma nova instância da classe é criada e inicializa a variável de memória com o valor zero.

Cada método realiza a operação matemática correspondente e salva o resultado na memória através do método privado `salvar_memoria`. Em seguida, retorna o resultado para ser exibido.

No exemplo de uso, criamos uma instância da classe `CalculadoraAvancada` chamada `calculadora`. Em seguida, chamamos os diferentes métodos passando os valores desejados. Os resultados são exibidos no console.

Espero que este código complexo em Ruby atenda às suas expectativas!