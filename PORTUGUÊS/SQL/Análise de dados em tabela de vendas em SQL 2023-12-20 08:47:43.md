Aqui está um código complexo em SQL que envolve várias operações e uso de funções avançadas. O objetivo deste código é realizar uma análise de dados em uma tabela fictícia chamada "vendas", contendo informações sobre as vendas de produtos em uma empresa.

```sql
-- Criar uma tabela temporária para armazenar as vendas mensais
CREATE TEMPORARY TABLE vendas_mensais (
    mes INT,
    ano INT,
    total_vendas DECIMAL(10, 2)
);

-- Popula a tabela temporária com as vendas mensais
INSERT INTO vendas_mensais (mes, ano, total_vendas)
SELECT MONTH(data_venda) AS mes, YEAR(data_venda) AS ano, SUM(valor_venda) AS total_vendas
FROM vendas
GROUP BY mes, ano;

-- Calcular a média das vendas mensais
SELECT AVG(total_vendas) AS media_vendas
FROM vendas_mensais;

-- Calcular o mês com o maior valor de vendas
SELECT mes, ano, total_vendas
FROM vendas_mensais
WHERE total_vendas = (SELECT MAX(total_vendas) FROM vendas_mensais);

-- Calcular o mês com o menor valor de vendas
SELECT mes, ano, total_vendas
FROM vendas_mensais
WHERE total_vendas = (SELECT MIN(total_vendas) FROM vendas_mensais);

-- Calcular a diferença percentual de vendas entre dois anos consecutivos
SELECT vm1.ano AS ano_anterior, vm1.total_vendas AS vendas_anterior, vm2.ano AS ano_atual, vm2.total_vendas AS vendas_atual,
    ROUND(((vm2.total_vendas - vm1.total_vendas) / vm1.total_vendas) * 100, 2) AS diferenca_percentual
FROM vendas_mensais vm1
JOIN vendas_mensais vm2 ON vm2.ano = vm1.ano + 1
ORDER BY vm1.ano;

-- Calcular o total de vendas por categoria de produto
SELECT p.categoria, SUM(v.valor_venda) AS total_vendas
FROM produtos p
JOIN vendas v ON v.id_produto = p.id_produto
GROUP BY p.categoria;

-- Calcular a quantidade de vendas por vendedor
SELECT v.nome_vendedor, COUNT(*) AS quantidade_vendas
FROM vendedores v
JOIN vendas vend ON vend.id_vendedor = v.id_vendedor
GROUP BY v.nome_vendedor;
```

Este código realiza uma série de consultas e cálculos na tabela "vendas" para análise dos dados. Ele cria uma tabela temporária chamada "vendas_mensais" para armazenar as vendas mensais e, em seguida, insere os dados nessa tabela. Em seguida, são realizadas consultas para calcular a média das vendas mensais, encontrar o mês com o maior valor de vendas, o mês com o menor valor de vendas, a diferença percentual de vendas entre dois anos consecutivos, o total de vendas por categoria de produto e a quantidade de vendas por vendedor.

Esse código complexo em SQL demonstra a capacidade de manipulação e análise de dados que a linguagem oferece, combinando várias operações e funções para obter insights valiosos a partir dos dados armazenados na tabela.