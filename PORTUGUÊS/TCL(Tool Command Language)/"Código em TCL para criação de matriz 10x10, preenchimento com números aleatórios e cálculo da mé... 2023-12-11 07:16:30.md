Aqui está um código complexo em TCL que cria uma matriz de 10x10, preenche com números aleatórios e calcula a média dos elementos:

```
# Definindo o tamanho da matriz
set linhas 10
set colunas 10

# Função para gerar um número aleatório entre 1 e 100
proc gerarAleatorio {} {
  return [expr {int(rand() * 100) + 1}]
}

# Criando a matriz
for {set i 0} {$i < $linhas} {incr i} {
  for {set j 0} {$j < $colunas} {incr j} {
    set matriz($i,$j) [gerarAleatorio]
  }
}

# Calculando a média dos elementos
set soma 0
for {set i 0} {$i < $linhas} {incr i} {
  for {set j 0} {$j < $colunas} {incr j} {
    set soma [expr {$soma + $matriz($i,$j)}]
  }
}

set media [expr {$soma / ($linhas * $colunas)}]

# Imprimindo a matriz e a média
puts "Matriz:"
for {set i 0} {$i < $linhas} {incr i} {
  for {set j 0} {$j < $colunas} {incr j} {
    puts -nonewline "$matriz($i,$j)\t"
  }
  puts ""
}

puts "Média: $media"
```

Explicação do código:

1. Definimos o tamanho da matriz como 10 linhas e 10 colunas.
2. Criamos uma função chamada `gerarAleatorio` que retorna um número aleatório entre 1 e 100.
3. Utilizamos dois loops `for` aninhados para percorrer cada posição da matriz e preenchê-la com um número aleatório gerado pela função `gerarAleatorio`.
4. Em seguida, utilizamos mais dois loops `for` aninhados para percorrer novamente cada posição da matriz e calcular a soma de todos os elementos.
5. Após a soma, calculamos a média dividindo a soma pelo total de elementos da matriz.
6. Por fim, imprimimos a matriz e a média na saída padrão.

Esse código é um exemplo de como gerar uma matriz e realizar operações básicas nela usando a linguagem TCL. Você pode modificá-lo conforme suas necessidades.