```portuguol
# Programa para calcular o fatorial de um número

número := 5

# Função para calcular o fatorial de um número. Ele recebe um número inteiro e retorna
# o fatorial desse número.
função fatorial(n) {
    se n = 0 então {
        retorne 1
    }
    senão {
        retorne n * fatorial(n-1)
    }
}

# Imprima o fatorial do número.
imprima "O fatorial de " número " é " fatorial(número)

# Função para verificar se um número é primo. Ele recebe um número inteiro e retorna
# se esse número é primo ou não.
função éPrimo(n) {
    se n = 1 então {
        retorne falso
    }
    senão {
        para divisor de 2 até raizQuadradaDoNúmero(n) {
            se n % divisor = 0 então {
                retorne falso
            }
        }
        retorne verdadeiro
    }
}

# Função para calcular a raiz quadrada de um número. Ele recebe um número inteiro e
# retorna a raiz quadrada desse número.
função raizQuadradaDoNúmero(n) {
    se n = 0 então {
        retorne 0
    }
    senão {
        quociente := n
        resto := n
        enquanto resto != 0 {
            resto := quociente % 2
            quociente := quociente / 2
        }
        retorne quociente
    }
}

# Imprima se o número é primo ou não.
se éPrimo(número) então {
    imprima "O número " número " é um número primo"
}
senão {
    imprima "O número " número " não é um número primo"
}
```

Esse código tem algumas funções úteis, como a função fatorial, a função éPrimo, e a função raizQuadradaDoNúmero. Essas funções podem ser usadas para realizar vários cálculos matemáticos e são muito úteis em vários programas.