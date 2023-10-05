Claro! Aqui está um código complexo em Lua que utiliza o conceito de recursividade para calcular o fatorial de um número:

```
-- Função recursiva para calcular o fatorial
function fatorial(n)
    if n == 0 then
        return 1
    else
        return n * fatorial(n - 1)
    end
end

-- Função principal
function main()
    print("Digite um número para calcular o fatorial:")
    local numero = tonumber(io.read()) -- Lê um número do usuário
    local resultado = fatorial(numero) -- Chama a função fatorial passando o número digitado
    print("O fatorial de " .. numero .. " é " .. resultado)
end

-- Chamada da função principal
main()
```

Neste código, temos duas funções: `fatorial` e `main`. A função `fatorial` é uma função recursiva que recebe um número `n` como parâmetro e calcula o fatorial desse número. Se `n` for igual a 0, a função retorna 1 (caso base da recursividade). Caso contrário, ela chama a si mesma passando `n-1` como argumento e multiplica o resultado por `n`. Dessa forma, a função se chama repetidamente até que `n` seja igual a 0.

A função `main` é a função principal do programa. Ela solicita ao usuário que digite um número, lê o número digitado e chama a função `fatorial` passando esse número. Em seguida, exibe o resultado do cálculo na tela.

Ao executar o código, você poderá digitar um número e o programa irá calcular o fatorial desse número. Por exemplo, se você digitar 5, o programa irá exibir "O fatorial de 5 é 120".