Claro! Aqui está um código em Python que gera um fractal chamado "Árvore de Pythagoras". Este fractal é construído a partir de uma base de triângulos retângulos isósceles, onde cada triângulo é gerado recursivamente a partir do triângulo anterior.

```python
import turtle

def arvore_pythagoras(t, comprimento, angulo, nivel):
    if nivel == 0:
        return
    else:
        t.forward(comprimento)
        t.left(angulo)
        arvore_pythagoras(t, comprimento * 0.7, angulo, nivel - 1)
        
        t.right(2 * angulo)
        arvore_pythagoras(t, comprimento * 0.7, angulo, nivel - 1)
        
        t.left(angulo)
        t.backward(comprimento)

# Configurações iniciais
t = turtle.Turtle()
t.speed(0)
t.left(90)
t.up()
t.backward(300)
t.down()

# Chamada da função para gerar a árvore de Pythagoras
arvore_pythagoras(t, 200, 45, 10)

# Finaliza a tela ao clicar nela
turtle.exitonclick()
```

Neste código, utilizamos a biblioteca Turtle para criar a representação visual da árvore de Pythagoras. A função `arvore_pythagoras` é responsável por gerar recursivamente os triângulos retângulos isósceles que compõem o fractal.

Os parâmetros da função são:
- `t`: um objeto da classe Turtle que representa a tartaruga para desenhar na tela.
- `comprimento`: o comprimento da base do triângulo retângulo isósceles.
- `angulo`: o ângulo entre a base e a hipotenusa do triângulo.
- `nivel`: o nível de recursão para gerar o fractal.

No início da função, verificamos se o nível é igual a zero. Caso seja, retornamos, o que indica o fim da recursão. Caso contrário, desenhamos um segmento de comprimento `comprimento`, viramos à esquerda em um ângulo `angulo` e chamamos a função novamente com um comprimento reduzido em 30% (`comprimento * 0.7`) e o mesmo ângulo e nível decrementado em 1.

Em seguida, viramos à direita em um ângulo `2 * angulo`, chamamos novamente a função com os mesmos parâmetros e, por fim, viramos à esquerda em `angulo` e retrocedemos o comprimento original.

Após definir a função, configuramos a tartaruga `t` com uma velocidade máxima (`speed(0)`), posição inicial e direção, e chamamos a função `arvore_pythagoras` com os argumentos desejados para gerar o fractal. Por fim, utilizamos `turtle.exitonclick()` para manter a tela aberta até que seja clicada.