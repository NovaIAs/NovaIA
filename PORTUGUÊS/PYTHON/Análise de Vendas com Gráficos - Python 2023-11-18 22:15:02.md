Claro! Aqui está um código complexo em Python que envolve processamento de dados, manipulação de strings e criação de gráficos. O código tem como objetivo ler um arquivo CSV contendo informações sobre vendas de um estabelecimento e gerar gráficos para análise dos dados. 

```python
import pandas as pd
import matplotlib.pyplot as plt

# Função para carregar os dados do arquivo CSV
def carregar_dados(nome_arquivo):
    dados = pd.read_csv(nome_arquivo)
    return dados

# Função para processar os dados
def processar_dados(dados):
    # Filtrar apenas as vendas realizadas em determinado mês
    vendas_mes = dados[dados['Data'].str.contains('2021-01')]
    
    # Calcular a receita total do mês
    receita_total = vendas_mes['Valor'].sum()
    
    # Calcular a quantidade total de vendas do mês
    quantidade_total = vendas_mes['Quantidade'].sum()
    
    # Criar um DataFrame com as informações de cada produto vendido
    info_produtos = vendas_mes.groupby('Produto').agg({'Valor': 'sum', 'Quantidade': 'sum'})
    
    return receita_total, quantidade_total, info_produtos

# Função para gerar os gráficos
def gerar_graficos(receita_total, quantidade_total, info_produtos):
    # Gráfico de barras com a receita por produto
    info_produtos['Valor'].plot(kind='bar')
    plt.xlabel('Produto')
    plt.ylabel('Receita')
    plt.title('Receita por produto')
    plt.show()
    
    # Gráfico de pizza com a quantidade de vendas por produto
    info_produtos['Quantidade'].plot(kind='pie', autopct='%1.1f%%')
    plt.ylabel('')
    plt.title('Quantidade de vendas por produto')
    plt.show()

# Nome do arquivo CSV contendo os dados de vendas
nome_arquivo = 'dados_vendas.csv'

# Carregar os dados do arquivo
dados = carregar_dados(nome_arquivo)

# Processar os dados
receita_total, quantidade_total, info_produtos = processar_dados(dados)

# Gerar os gráficos
gerar_graficos(receita_total, quantidade_total, info_produtos)
```

Neste código, utilizamos a biblioteca `pandas` para carregar os dados do arquivo CSV e manipulá-los. Utilizamos também a biblioteca `matplotlib.pyplot` para criar os gráficos. O código é dividido em três funções principais:

- A função `carregar_dados` recebe o nome de um arquivo CSV e retorna um DataFrame com os dados do arquivo.
- A função `processar_dados` recebe o DataFrame com as informações de vendas e realiza o processamento necessário, filtrando as vendas de um determinado mês, calculando a receita total e a quantidade total de vendas, e criando um novo DataFrame com as informações de cada produto vendido.
- A função `gerar_graficos` recebe os resultados do processamento e utiliza a biblioteca `matplotlib` para gerar dois gráficos: um gráfico de barras com a receita por produto e um gráfico de pizza com a quantidade de vendas por produto.

Por fim, o código carrega os dados do arquivo CSV, processa os dados e gera os gráficos para análise das informações de vendas.