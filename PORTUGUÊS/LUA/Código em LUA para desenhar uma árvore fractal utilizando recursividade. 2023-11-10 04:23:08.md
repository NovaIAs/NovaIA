Claro, aqui está um código em LUA que utiliza recursividade para criar uma árvore fractal:

```
-- Define as variáveis iniciais
local largura = 400
local altura = 400
local tamanho = 150
local angulo = 20
local nivel = 5

-- Função para desenhar a árvore fractal
local function desenharArvore(x, y, ang, tam, n)
    if n == 0 then
        return
    end
    -- calcula a posição final do ramo
    local xf = x + tam * math.cos(math.rad(ang))
    local yf = y - tam * math.sin(math.rad(ang))
    -- desenha o ramo
    love.graphics.line(x, y, xf, yf)
    -- calcula os ramos filhos
    local tamFilho = tam * 0.67
    local nFilho = n - 1
    local angEsq = ang + angulo
    local angDir = ang - angulo
    -- chama a função recursivamente para desenhar os ramos filhos
    desenharArvore(xf, yf, angEsq, tamFilho, nFilho)
    desenharArvore(xf, yf, angDir, tamFilho, nFilho)
end

-- Função para desenhar a árvore fractal na tela
function love.draw()
    love.graphics.setBackgroundColor(255, 255, 255)
    love.graphics.setColor(0, 0, 0)
    -- desenha a árvore fractal
    desenharArvore(largura/2, altura, -90, tamanho, nivel)
end
```

Esse código utiliza a biblioteca Love2D para desenhar na tela. A função principal é `desenharArvore`, que recebe os seguintes parâmetros:

- `x` e `y`: as coordenadas iniciais do ramo
- `ang`: o ângulo em graus em relação ao eixo Y que o ramo deve ser desenhado
- `tam`: o tamanho do ramo
- `n`: o nível atual da recursividade

A função começa testando se `n` é igual a zero, o que significa que não há mais níveis de recursividade a serem desenhados. Se `n` não for zero, a função calcula a posição final do ramo e desenha uma linha entre a posição inicial e final. Em seguida, a função chama a si mesma duas vezes com novos parâmetros para desenhar os ramos filhos. Essa chamada recursiva é feita para os dois ramos filhos, com um ângulo `angEsq` para o ramo da esquerda e `angDir` para o ramo da direita.

O código completo desenha uma árvore fractal com 5 níveis de recursividade, onde cada ramo é 2/3 do tamanho do ramo anterior e tem um ângulo de 20 graus em relação ao ramo anterior. O resultado é uma imagem complexa e diferenciada que dificilmente se repetirá de forma idêntica.