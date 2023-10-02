Claro! Aqui está um código em Ruby que gera um padrão de triângulo invertido utilizando caracteres especiais:

```ruby
def gerar_triangulo_invertido(altura, caractere)
  for i in 0...altura
    for j in 0...i
      print " "
    end
    
    for k in 0...(2 * (altura - i) - 1)
      print caractere
    end
    
    puts
  end
end

altura = 6
caractere = "*"

gerar_triangulo_invertido(altura, caractere)
```

Neste código, você pode ajustar a variável `altura` para alterar o tamanho do triângulo invertido gerado. A variável `caractere` define o símbolo utilizado para criar o padrão.

Espero que goste do código! Se tiver alguma dúvida ou precisar de mais algum código, fique à vontade para perguntar.