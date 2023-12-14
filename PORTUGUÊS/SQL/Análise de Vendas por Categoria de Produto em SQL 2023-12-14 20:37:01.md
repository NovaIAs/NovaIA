Claro! Aqui está um exemplo de um código em SQL que realiza uma análise de dados de uma empresa fictícia de vendas:

```
-- Criação das tabelas
CREATE TABLE Cliente (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200),
    cidade VARCHAR(100),
    estado VARCHAR(50)
);

CREATE TABLE Produto (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10,2),
    categoria VARCHAR(50)
);

CREATE TABLE Venda (
    id INT PRIMARY KEY,
    cliente_id INT,
    produto_id INT,
    data_venda DATE,
    quantidade INT,
    valor_total DECIMAL(10,2),
    FOREIGN KEY (cliente_id) REFERENCES Cliente(id),
    FOREIGN KEY (produto_id) REFERENCES Produto(id)
);

-- Inserção de dados nas tabelas
INSERT INTO Cliente (id, nome, endereco, cidade, estado)
VALUES (1, 'João da Silva', 'Rua A, 123', 'São Paulo', 'SP');

INSERT INTO Cliente (id, nome, endereco, cidade, estado)
VALUES (2, 'Maria Souza', 'Av. B, 456', 'Rio de Janeiro', 'RJ');

INSERT INTO Produto (id, nome, preco, categoria)
VALUES (1, 'Camiseta', 50.00, 'Vestuário');

INSERT INTO Produto (id, nome, preco, categoria)
VALUES (2, 'Celular', 1500.00, 'Eletrônicos');

INSERT INTO Venda (id, cliente_id, produto_id, data_venda, quantidade, valor_total)
VALUES (1, 1, 1, '2022-01-01', 2, 100.00);

INSERT INTO Venda (id, cliente_id, produto_id, data_venda, quantidade, valor_total)
VALUES (2, 2, 2, '2022-01-02', 1, 1500.00);

-- Consulta para obter o valor total de vendas por categoria de produto
SELECT p.categoria, SUM(v.valor_total) AS total_vendas
FROM Venda v
JOIN Produto p ON v.produto_id = p.id
GROUP BY p.categoria;
```

Neste código, começamos criando três tabelas: `Cliente`, `Produto` e `Venda`. Em seguida, inserimos alguns dados nessas tabelas usando a cláusula `INSERT INTO`.

A parte mais interessante do código é a consulta realizada no final. Utilizamos uma cláusula `SELECT` para selecionar a categoria de produto e a soma do valor total de vendas para cada categoria. Para isso, utilizamos as tabelas `Venda` e `Produto`, realizando um `JOIN` entre elas para relacionar as vendas aos produtos correspondentes. Por fim, agrupamos o resultado pelo campo `p.categoria` usando a cláusula `GROUP BY`.

Esse código é apenas um exemplo simples, mas pode ser adaptado e expandido para atender às necessidades específicas de uma empresa real.