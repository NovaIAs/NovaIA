Aqui está um código complexo em SQL que realiza uma análise de dados de vendas em uma empresa fictícia de eletrônicos. O código é bastante extenso e contém várias etapas para processar e analisar os dados. Vou explicar cada parte do código para facilitar o entendimento.

```sql
-- Criação da tabela de produtos
CREATE TABLE Produtos (
    Id INT PRIMARY KEY,
    Nome VARCHAR(100),
    Preco DECIMAL(10,2),
    Categoria VARCHAR(50)
);

-- Criação da tabela de vendas
CREATE TABLE Vendas (
    Id INT PRIMARY KEY,
    Data DATE,
    ProdutoId INT,
    Quantidade INT,
    ValorTotal DECIMAL(10,2),
    FOREIGN KEY (ProdutoId) REFERENCES Produtos(Id)
);

-- Inserção de dados na tabela de produtos
INSERT INTO Produtos (Id, Nome, Preco, Categoria) VALUES
    (1, 'Smartphone X', 1500.00, 'Celulares'),
    (2, 'TV LED 40 polegadas', 2000.00, 'TVs'),
    (3, 'Notebook Y', 3000.00, 'Notebooks'),
    (4, 'Fone de Ouvido Z', 100.00, 'Acessórios');

-- Inserção de dados na tabela de vendas
INSERT INTO Vendas (Id, Data, ProdutoId, Quantidade, ValorTotal) VALUES
    (1, '2021-01-01', 1, 10, 15000.00),
    (2, '2021-01-02', 2, 5, 10000.00),
    (3, '2021-01-03', 3, 2, 6000.00),
    (4, '2021-01-04', 1, 5, 7500.00),
    (5, '2021-01-05', 4, 20, 2000.00);

-- Consulta para obter o total de vendas por categoria
SELECT p.Categoria, SUM(v.ValorTotal) AS TotalVendas
FROM Produtos p
JOIN Vendas v ON p.Id = v.ProdutoId
GROUP BY p.Categoria;

-- Consulta para obter o produto mais vendido
SELECT p.Nome, SUM(v.Quantidade) AS TotalVendido
FROM Produtos p
JOIN Vendas v ON p.Id = v.ProdutoId
GROUP BY p.Nome
ORDER BY TotalVendido DESC
LIMIT 1;

-- Consulta para obter a média de preço dos produtos por categoria
SELECT p.Categoria, AVG(p.Preco) AS MediaPreco
FROM Produtos p
GROUP BY p.Categoria;
```

Primeiramente, o código cria duas tabelas: `Produtos` e `Vendas`. A tabela `Produtos` armazena informações sobre os produtos, como ID, nome, preço e categoria. A tabela `Vendas` registra as vendas realizadas, contendo informações como ID, data, ID do produto vendido, quantidade e valor total da venda. 

Em seguida, são inseridos alguns dados de exemplo nas tabelas utilizando a instrução `INSERT INTO`.

Após a inserção dos dados, são realizadas três consultas para análise dos dados:

1. A primeira consulta obtém o total de vendas por categoria. Isso é feito através de um join entre as tabelas `Produtos` e `Vendas`, agrupando os resultados pela categoria do produto e somando o valor total de vendas para cada categoria.

2. A segunda consulta identifica o produto mais vendido. Ela também utiliza um join entre as tabelas `Produtos` e `Vendas`, agrupando os resultados pelo nome do produto e ordenando-os em ordem decrescente de quantidade vendida. O resultado é limitado a apenas 1 registro para obter o produto mais vendido.

3. Por fim, a terceira consulta calcula a média de preço dos produtos por categoria. Ela agrupa os produtos pela categoria e utiliza a função de agregação AVG para calcular a média do preço dos produtos em cada categoria.

Espero que esse código complexo em SQL atenda às suas expectativas! Se você tiver mais alguma solicitação, fique à vontade para pedir.