Claro! Aqui está um código complexo em SQL que realiza uma análise de dados de vendas de uma empresa fictícia. O código irá calcular o total de vendas, a média de vendas mensais e a quantidade de clientes que fizeram compras em cada mês.

```sql
-- Criação de uma tabela temporária para armazenar os dados de vendas
CREATE TEMPORARY TABLE vendas (
    id_cliente INT,
    data_venda DATE,
    valor_venda DECIMAL(10,2)
);

-- Inserção de dados de exemplo na tabela vendas
INSERT INTO vendas (id_cliente, data_venda, valor_venda)
VALUES
    (1, '2021-01-05', 100.50),
    (2, '2021-01-10', 75.20),
    (3, '2021-01-15', 50.80),
    (1, '2021-02-05', 200.30),
    (2, '2021-02-10', 150.50),
    (3, '2021-02-15', 100.75),
    (4, '2021-02-20', 300.25),
    (1, '2021-03-05', 150.80),
    (2, '2021-03-15', 125.60),
    (3, '2021-03-20', 90.10),
    (4, '2021-03-25', 200.50),
    (5, '2021-03-30', 175.25);

-- Cálculo do total de vendas
SELECT SUM(valor_venda) AS total_vendas
FROM vendas;

-- Cálculo da média de vendas mensais
SELECT EXTRACT(MONTH FROM data_venda) AS mes,
       AVG(valor_venda) AS media_vendas
FROM vendas
GROUP BY EXTRACT(MONTH FROM data_venda);

-- Contagem de clientes que fizeram compras em cada mês
SELECT EXTRACT(MONTH FROM data_venda) AS mes,
       COUNT(DISTINCT id_cliente) AS quantidade_clientes
FROM vendas
GROUP BY EXTRACT(MONTH FROM data_venda);
```

Este código começa criando uma tabela temporária chamada "vendas" para armazenar os dados de vendas. Em seguida, são inseridos dados de exemplo na tabela.

Depois disso, há três consultas SQL distintas:

1. A primeira consulta calcula o total de vendas, somando os valores de todas as vendas registradas na tabela "vendas".

2. A segunda consulta calcula a média de vendas mensais. Ela utiliza a função EXTRACT para obter o mês a partir da data de venda e agrupa os valores por mês. A média das vendas é calculada para cada mês.

3. A terceira consulta conta a quantidade de clientes que fizeram compras em cada mês. Ela também utiliza a função EXTRACT para obter o mês a partir da data de venda, agrupa os valores por mês e conta a quantidade de clientes distintos.

Essas consultas fornecem informações úteis sobre as vendas da empresa, permitindo uma análise mais detalhada dos dados.