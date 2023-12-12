Claro! Aqui está um código complexo em Python que envolve manipulação de dados, estruturas de controle e funções personalizadas:

```python
# Este código é um exemplo de análise de dados de vendas

# Definindo uma função para calcular a média
def calcular_media(lista):
    soma = sum(lista)
    media = soma / len(lista)
    return media

# Definindo uma função para calcular o total de vendas
def calcular_total_vendas(vendas):
    total = 0
    for venda in vendas:
        total += venda['valor']
    return total

# Definindo uma função para encontrar o mês com maior valor de vendas
def encontrar_mes_maior_vendas(vendas):
    mes_maior_vendas = vendas[0]['mes']
    valor_maior_vendas = vendas[0]['valor']
    for venda in vendas:
        if venda['valor'] > valor_maior_vendas:
            valor_maior_vendas = venda['valor']
            mes_maior_vendas = venda['mes']
    return mes_maior_vendas

# Dados de vendas mensais
vendas = [
    {'mes': 'Janeiro', 'valor': 5000},
    {'mes': 'Fevereiro', 'valor': 6000},
    {'mes': 'Março', 'valor': 4500},
    {'mes': 'Abril', 'valor': 7000},
    {'mes': 'Maio', 'valor': 5500},
    {'mes': 'Junho', 'valor': 8000},
]

# Calculando o total de vendas
total_vendas = calcular_total_vendas(vendas)
print('O total de vendas é:', total_vendas)

# Calculando a média de vendas
valores_vendas = [venda['valor'] for venda in vendas]
media_vendas = calcular_media(valores_vendas)
print('A média de vendas é:', media_vendas)

# Encontrando o mês com maior valor de vendas
mes_maior_vendas = encontrar_mes_maior_vendas(vendas)
print('O mês com maior valor de vendas é:', mes_maior_vendas)
```

Neste código, temos uma lista de vendas mensais representada por dicionários dentro da lista `vendas`. A função `calcular_total_vendas` percorre essa lista e soma todos os valores de venda. A função `calcular_media` recebe uma lista de valores e calcula a média. A função `encontrar_mes_maior_vendas` percorre a lista de vendas e encontra o mês com o maior valor de vendas.

No programa principal, chamamos as funções para obter o total de vendas, a média de vendas e o mês com maior valor de vendas. Os resultados são então exibidos na tela.