Claro! Aqui está um exemplo de um código SQL complexo que realiza uma análise de dados em uma tabela de vendas de uma loja fictícia:

```
-- Criação de tabela temporária para armazenar as vendas por categoria
CREATE TABLE #VendasPorCategoria (
    Categoria VARCHAR(50),
    TotalVendas INT
);

-- Inserção dos dados na tabela temporária
INSERT INTO #VendasPorCategoria
SELECT Categoria, COUNT(*) AS TotalVendas
FROM Vendas
GROUP BY Categoria
ORDER BY TotalVendas DESC;

-- Criação de tabela temporária para armazenar as vendas por mês
CREATE TABLE #VendasPorMes (
    Mes INT,
    TotalVendas INT
);

-- Inserção dos dados na tabela temporária
INSERT INTO #VendasPorMes
SELECT MONTH(DataVenda) AS Mes, COUNT(*) AS TotalVendas
FROM Vendas
GROUP BY MONTH(DataVenda)
ORDER BY Mes ASC;

-- Criação de tabela temporária para armazenar as vendas por dia da semana
CREATE TABLE #VendasPorDiaSemana (
    DiaSemana VARCHAR(20),
    TotalVendas INT
);

-- Inserção dos dados na tabela temporária
INSERT INTO #VendasPorDiaSemana
SELECT DATENAME(WEEKDAY, DataVenda) AS DiaSemana, COUNT(*) AS TotalVendas
FROM Vendas
GROUP BY DATENAME(WEEKDAY, DataVenda)
ORDER BY TotalVendas DESC;

-- Consulta para obter o total de vendas por categoria
SELECT *
FROM #VendasPorCategoria;

-- Consulta para obter o total de vendas por mês
SELECT *
FROM #VendasPorMes;

-- Consulta para obter o total de vendas por dia da semana
SELECT *
FROM #VendasPorDiaSemana;

-- Exclusão das tabelas temporárias
DROP TABLE #VendasPorCategoria;
DROP TABLE #VendasPorMes;
DROP TABLE #VendasPorDiaSemana;
```

Neste exemplo, o código começa criando três tabelas temporárias: `#VendasPorCategoria`, `#VendasPorMes` e `#VendasPorDiaSemana`. Essas tabelas serão usadas para armazenar os resultados das consultas.

Em seguida, o código insere os dados nessas tabelas temporárias usando a cláusula `INSERT INTO` e a função `COUNT(*)` para contar o número de vendas por categoria, mês e dia da semana.

Após inserir os dados, as tabelas temporárias são consultadas individualmente para obter os totais de vendas por categoria, mês e dia da semana.

Por fim, as tabelas temporárias são excluídas usando a cláusula `DROP TABLE`.

Espero que este código atenda às suas expectativas!