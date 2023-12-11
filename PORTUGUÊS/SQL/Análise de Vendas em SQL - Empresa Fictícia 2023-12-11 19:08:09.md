Claro! Aqui está um código complexo em SQL que realiza uma análise de dados de vendas de uma empresa fictícia. Ele é bastante detalhado e contém explicações em cada etapa para facilitar o entendimento:

```
-- Criação de tabela para armazenar as informações de vendas
CREATE TABLE vendas (
    id_venda INT PRIMARY KEY,
    data_venda DATE,
    id_produto INT,
    quantidade INT,
    valor_total DECIMAL(10, 2)
);

-- Inserção de dados de exemplo na tabela de vendas
INSERT INTO vendas (id_venda, data_venda, id_produto, quantidade, valor_total)
VALUES
    (1, '2022-01-01', 1, 10, 100.00),
    (2, '2022-01-01', 2, 5, 50.00),
    (3, '2022-01-02', 1, 7, 70.00),
    (4, '2022-01-02', 2, 3, 30.00),
    (5, '2022-01-03', 3, 2, 20.00),
    (6, '2022-01-03', 4, 8, 80.00);

-- Consulta para obter o total de vendas por produto
SELECT
    p.id_produto,
    p.nome_produto,
    SUM(v.quantidade) AS total_quantidade,
    SUM(v.valor_total) AS total_valor
FROM
    vendas v
    JOIN produtos p ON v.id_produto = p.id_produto
GROUP BY
    p.id_produto,
    p.nome_produto;

-- Consulta para obter o total de vendas por dia
SELECT
    data_venda,
    COUNT(*) AS total_vendas
FROM
    vendas
GROUP BY
    data_venda;

-- Consulta para obter o total de vendas por mês
SELECT
    EXTRACT(MONTH FROM data_venda) AS mes,
    EXTRACT(YEAR FROM data_venda) AS ano,
    COUNT(*) AS total_vendas
FROM
    vendas
GROUP BY
    mes,
    ano
ORDER BY
    ano,
    mes;
```

Explicações:

1. Começamos criando uma tabela chamada "vendas" para armazenar as informações de vendas da empresa.
2. Em seguida, inserimos alguns dados de exemplo na tabela de vendas.
3. A primeira consulta realiza uma análise de vendas por produto, somando a quantidade e o valor total de vendas para cada produto. Ela utiliza um join com a tabela de produtos para obter o nome do produto.
4. A segunda consulta calcula o total de vendas por dia, contando o número de vendas para cada data de venda.
5. A terceira consulta calcula o total de vendas por mês, utilizando a função EXTRACT para extrair o mês e o ano da data de venda. Os resultados são ordenados por ano e mês.

Espero que esse código atenda às suas expectativas!