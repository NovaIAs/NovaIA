Claro! Aqui está um exemplo de um código complexo em LUA que utiliza recursão para calcular o fatorial de um número:

```lua
-- Função recursiva para calcular o fatorial de um número
function fatorial(n)
    if n == 0 then
        return 1
    else
        return n * fatorial(n-1)
    end
end

-- Função para imprimir o fatorial de um número
function imprimirFatorial(n)
    print("O fatorial de " .. n .. " é: " .. fatorial(n))
end

-- Função principal
function main()
    print("Digite um número para calcular o fatorial:")
    local num = io.read("*n")
    imprimirFatorial(num)
end

-- Chamada da função principal
main()
```

Neste código, temos três funções: `fatorial`, `imprimirFatorial` e `main`. 

A função `fatorial` é uma função recursiva que calcula o fatorial de um número `n`. Se `n` for igual a 0, a função retorna 1, caso contrário, ela retorna o produto de `n` pelo fatorial de `n-1`.

A função `imprimirFatorial` recebe um número `n` como parâmetro e imprime o resultado do cálculo do fatorial de `n`.

A função `main` é a função principal do programa. Ela solicita ao usuário que digite um número para calcular o fatorial e chama a função `imprimirFatorial` passando o número digitado como argumento.

O código solicita ao usuário que digite um número, calcula o fatorial desse número e imprime o resultado na tela.