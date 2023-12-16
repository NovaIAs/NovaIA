Claro! Aqui está um código complexo em SQL que realiza uma análise de dados de uma empresa fictícia de vendas. O código utiliza uma combinação de consultas, subconsultas e funções de agregação para extrair informações relevantes do banco de dados.

```sql
-- Criação de tabela de vendas
CREATE TABLE vendas (
    id_venda INT PRIMARY KEY,
    id_produto INT,
    id_cliente INT,
    data_venda DATE,
    valor DECIMAL(10, 2)
);

-- Preenchimento de tabela de vendas com dados fictícios
INSERT INTO vendas (id_venda, id_produto, id_cliente, data_venda, valor)
VALUES
    (1, 1, 1, '2021-01-01', 100.00),
    (2, 2, 2, '2021-01-02', 200.00),
    (3, 1, 3, '2021-01-03', 150.00),
    (4, 3, 4, '2021-01-04', 120.00),
    (5, 2, 5, '2021-01-05', 180.00),
    (6, 1, 6, '2021-01-06', 130.00),
    (7, 4, 7, '2021-01-07', 90.00),
    (8, 3, 8, '2021-01-08', 110.00),
    (9, 1, 9, '2021-01-09', 160.00),
    (10, 2, 10, '2021-01-10', 220.00);

-- Consulta que retorna o total de vendas por produto
SELECT 
    p.nome AS produto,
    COUNT(v.id_venda) AS total_vendas,
    SUM(v.valor) AS total_valor
FROM 
    vendas v
    INNER JOIN produtos p ON v.id_produto = p.id_produto
GROUP BY 
    p.nome;

-- Consulta que retorna o total de vendas por cliente
SELECT 
    c.nome AS cliente,
    COUNT(v.id_venda) AS total_vendas,
    SUM(v.valor) AS total_valor
FROM 
    vendas v
    INNER JOIN clientes c ON v.id_cliente = c.id_cliente
GROUP BY 
    c.nome;

-- Consulta que retorna a média de vendas e o total de vendas por mês
SELECT 
    MONTH(v.data_venda) AS mes,
    YEAR(v.data_venda) AS ano,
    COUNT(v.id_venda) AS total_vendas,
    AVG(v.valor) AS media_valor
FROM 
    vendas v
GROUP BY 
    MONTH(v.data_venda),
    YEAR(v.data_venda);

-- Consulta que retorna o produto mais vendido
SELECT 
    p.nome AS produto,
    COUNT(v.id_venda) AS total_vendas
FROM 
    vendas v
    INNER JOIN produtos p ON v.id_produto = p.id_produto
GROUP BY 
    p.nome
HAVING 
    COUNT(v.id_venda) = (
        SELECT MAX(total_vendas)
        FROM (
            SELECT 
                COUNT(id_venda) AS total_vendas
            FROM 
                vendas
            GROUP BY 
                id_produto
        ) AS subquery
    );

-- Consulta que retorna o cliente que mais gastou
SELECT 
    c.nome AS cliente,
    SUM(v.valor) AS total_gasto
FROM 
    vendas v
    INNER JOIN clientes c ON v.id_cliente = c.id_cliente
GROUP BY 
    c.nome
HAVING 
    SUM(v.valor) = (
        SELECT MAX(total_gasto)
        FROM (
            SELECT 
                SUM(valor) AS total_gasto
            FROM 
                vendas
            GROUP BY 
                id_cliente
        ) AS subquery
    );
```

Esse código cria uma tabela chamada "vendas" que armazena informações sobre as vendas da empresa. Em seguida, preenche a tabela com alguns dados fictícios.

Após a criação e preenchimento da tabela, o código contém várias consultas que analisam os dados e retornam informações relevantes. A primeira consulta retorna o total de vendas por produto, mostrando o nome do produto, o número total de vendas e o valor total das vendas.

A segunda consulta retorna o total de vendas por cliente, mostrando o nome do cliente, o número total de vendas e o valor total das vendas.

A terceira consulta retorna a média de vendas e o total de vendas por mês, mostrando o mês, o ano, o número total de vendas e a média do valor das vendas.

A quarta consulta retorna o produto mais vendido, mostrando o nome do produto e o número total de vendas. Essa consulta utiliza uma subconsulta para determinar o produto com o maior número de vendas.

A quinta consulta retorna o cliente que mais gastou, mostrando o nome do cliente e o valor total gasto. Essa consulta também utiliza uma subconsulta para determinar o cliente com o maior valor de gastos.

Essas consultas podem ser adaptadas e modificadas de acordo com as necessidades específicas da empresa.