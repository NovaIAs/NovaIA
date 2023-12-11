Claro! Aqui está um exemplo de um código complexo em SQL para criação de um sistema de gerenciamento de vendas em uma loja.

```
-- Criação das Tabelas
CREATE TABLE Cliente (
  id INT PRIMARY KEY,
  nome VARCHAR(100),
  endereco VARCHAR(200),
  telefone VARCHAR(20)
);

CREATE TABLE Produto (
  id INT PRIMARY KEY,
  nome VARCHAR(100),
  preco DECIMAL(10, 2),
  quantidade INT
);

CREATE TABLE Venda (
  id INT PRIMARY KEY,
  data_venda DATE,
  id_cliente INT,
  FOREIGN KEY (id_cliente) REFERENCES Cliente(id)
);

CREATE TABLE ItemVenda (
  id INT PRIMARY KEY,
  id_venda INT,
  id_produto INT,
  quantidade INT,
  FOREIGN KEY (id_venda) REFERENCES Venda(id),
  FOREIGN KEY (id_produto) REFERENCES Produto(id)
);

-- Inserção de Dados
INSERT INTO Cliente (id, nome, endereco, telefone)
VALUES (1, 'João Silva', 'Rua A, 123', '9999-1234');

INSERT INTO Cliente (id, nome, endereco, telefone)
VALUES (2, 'Maria Santos', 'Av. B, 456', '8888-5678');

INSERT INTO Produto (id, nome, preco, quantidade)
VALUES (1, 'Camiseta', 29.90, 100);

INSERT INTO Produto (id, nome, preco, quantidade)
VALUES (2, 'Calça Jeans', 99.90, 50);

-- Realização de Vendas
INSERT INTO Venda (id, data_venda, id_cliente)
VALUES (1, '2022-01-01', 1);

INSERT INTO ItemVenda (id, id_venda, id_produto, quantidade)
VALUES (1, 1, 1, 2);

INSERT INTO ItemVenda (id, id_venda, id_produto, quantidade)
VALUES (2, 1, 2, 1);

-- Consultas
-- Listar todas as vendas com seus respectivos itens
SELECT v.id AS venda_id, v.data_venda, c.nome AS cliente_nome, p.nome AS produto_nome, iv.quantidade, p.preco
FROM Venda v
INNER JOIN Cliente c ON v.id_cliente = c.id
INNER JOIN ItemVenda iv ON v.id = iv.id_venda
INNER JOIN Produto p ON iv.id_produto = p.id;

-- Consultar o total de vendas por cliente
SELECT c.nome AS cliente_nome, COUNT(v.id) AS total_vendas
FROM Cliente c
LEFT JOIN Venda v ON c.id = v.id_cliente
GROUP BY c.nome;

-- Consultar o total de vendas por produto
SELECT p.nome AS produto_nome, SUM(iv.quantidade) AS total_vendas
FROM Produto p
LEFT JOIN ItemVenda iv ON p.id = iv.id_produto
GROUP BY p.nome;
```

Neste exemplo, começamos criando as tabelas necessárias para armazenar os dados de clientes, produtos, vendas e itens de venda. Em seguida, inserimos alguns registros de exemplo em cada tabela.

Após isso, realizamos algumas consultas para visualizar os dados. Primeiro, listamos todas as vendas com seus respectivos itens, exibindo o ID da venda, data, nome do cliente, nome do produto, quantidade e preço. Em seguida, consultamos o total de vendas por cliente, agrupando pelo nome do cliente e contando o número de vendas. Por fim, consultamos o total de vendas por produto, agrupando pelo nome do produto e somando a quantidade vendida.

Espero que este exemplo atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais detalhes, estou à disposição.