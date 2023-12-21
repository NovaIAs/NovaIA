Claro! Aqui está um exemplo de código SQL complexo que envolve consultas, junções, subconsultas e agregações para análise de dados de uma empresa fictícia de comércio eletrônico:

```sql
-- Obter a quantidade total de pedidos e a receita total por categoria de produto

-- Crie uma tabela temporária para armazenar a quantidade de pedidos por categoria
CREATE TEMPORARY TABLE IF NOT EXISTS quantidade_pedidos_categoria AS (
  SELECT p.id_categoria, COUNT(DISTINCT o.id_pedido) AS quantidade_pedidos
  FROM pedidos o
  INNER JOIN itens_pedido i ON i.id_pedido = o.id_pedido
  INNER JOIN produtos p ON p.id_produto = i.id_produto
  GROUP BY p.id_categoria
);

-- Crie uma tabela temporária para armazenar a receita total por categoria
CREATE TEMPORARY TABLE IF NOT EXISTS receita_total_categoria AS (
  SELECT p.id_categoria, SUM(i.preco_unitario * i.quantidade) AS receita_total
  FROM pedidos o
  INNER JOIN itens_pedido i ON i.id_pedido = o.id_pedido
  INNER JOIN produtos p ON p.id_produto = i.id_produto
  GROUP BY p.id_categoria
);

-- Obtenha a quantidade total de pedidos e a receita total por categoria
SELECT c.nome AS categoria, q.quantidade_pedidos, r.receita_total
FROM quantidade_pedidos_categoria q
INNER JOIN receita_total_categoria r ON r.id_categoria = q.id_categoria
INNER JOIN categorias c ON c.id_categoria = q.id_categoria;
```

Neste exemplo, estamos analisando os dados de uma tabela de pedidos, itens de pedido, produtos e categorias. O objetivo é obter a quantidade total de pedidos e a receita total por categoria de produto.

Primeiro, criamos duas tabelas temporárias: "quantidade_pedidos_categoria" e "receita_total_categoria". A tabela "quantidade_pedidos_categoria" armazena a quantidade de pedidos por categoria, enquanto a tabela "receita_total_categoria" armazena a receita total por categoria.

Em seguida, usamos as junções para combinar as tabelas de pedidos, itens de pedido, produtos e categorias. Utilizamos as agregações para contar a quantidade de pedidos e calcular a receita total por categoria.

Por fim, selecionamos as colunas relevantes (nome da categoria, quantidade de pedidos e receita total) e fazemos as junções necessárias para obter os resultados desejados.

Esse código SQL complexo permite obter informações valiosas sobre o desempenho de vendas por categoria de produto em uma empresa de comércio eletrônico.