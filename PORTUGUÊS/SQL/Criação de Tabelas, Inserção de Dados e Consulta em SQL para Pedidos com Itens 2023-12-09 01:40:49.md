Claro! Aqui está um código complexo em SQL que envolve várias tabelas e operações:

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS Cliente (
    idCliente INT PRIMARY KEY,
    nome VARCHAR(50),
    email VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS Produto (
    idProduto INT PRIMARY KEY,
    nome VARCHAR(50),
    preco DECIMAL(10, 2)
);

CREATE TABLE IF NOT EXISTS Pedido (
    idPedido INT PRIMARY KEY,
    idCliente INT,
    dataPedido DATE,
    valorTotal DECIMAL(10, 2),
    FOREIGN KEY (idCliente) REFERENCES Cliente(idCliente)
);

CREATE TABLE IF NOT EXISTS ItemPedido (
    idItemPedido INT PRIMARY KEY,
    idPedido INT,
    idProduto INT,
    quantidade INT,
    precoUnitario DECIMAL(10, 2),
    FOREIGN KEY (idPedido) REFERENCES Pedido(idPedido),
    FOREIGN KEY (idProduto) REFERENCES Produto(idProduto)
);

-- Inserção de dados nas tabelas
INSERT INTO Cliente (idCliente, nome, email)
VALUES (1, 'João da Silva', 'joao.silva@example.com');

INSERT INTO Produto (idProduto, nome, preco)
VALUES (1, 'Camiseta', 49.90),
       (2, 'Calça Jeans', 99.90),
       (3, 'Tênis', 149.90);

INSERT INTO Pedido (idPedido, idCliente, dataPedido, valorTotal)
VALUES (1, 1, '2022-01-01', 0),
       (2, 1, '2022-02-01', 0);

-- Atualização do valorTotal nos pedidos
UPDATE Pedido
SET valorTotal = (
    SELECT SUM(quantidade * precoUnitario)
    FROM ItemPedido
    WHERE ItemPedido.idPedido = Pedido.idPedido
)
WHERE EXISTS (
    SELECT 1
    FROM ItemPedido
    WHERE ItemPedido.idPedido = Pedido.idPedido
);

-- Inserção de itens nos pedidos
INSERT INTO ItemPedido (idItemPedido, idPedido, idProduto, quantidade, precoUnitario)
VALUES (1, 1, 1, 2, 49.90),
       (2, 1, 3, 1, 149.90),
       (3, 2, 2, 1, 99.90);

-- Consulta dos pedidos com valor total e itens
SELECT
    Pedido.idPedido,
    Cliente.nome AS nomeCliente,
    Pedido.dataPedido,
    Pedido.valorTotal,
    Produto.nome AS nomeProduto,
    ItemPedido.quantidade,
    ItemPedido.precoUnitario
FROM Pedido
JOIN Cliente ON Cliente.idCliente = Pedido.idCliente
JOIN ItemPedido ON ItemPedido.idPedido = Pedido.idPedido
JOIN Produto ON Produto.idProduto = ItemPedido.idProduto;
```

Este código em SQL cria quatro tabelas: Cliente, Produto, Pedido e ItemPedido. Em seguida, insere alguns dados nessas tabelas. A tabela Cliente armazena informações sobre os clientes, como nome e email. A tabela Produto armazena informações sobre os produtos, como nome e preço. A tabela Pedido armazena informações sobre os pedidos, como o cliente associado, a data e o valor total. A tabela ItemPedido armazena informações sobre os itens de cada pedido, como o produto associado, a quantidade e o preço unitário.

Após a inserção dos dados, o código realiza uma atualização do valor total nos pedidos, calculando a soma do preço unitário multiplicado pela quantidade de cada item de pedido. Em seguida, são inseridos alguns itens nos pedidos.

Por fim, o código realiza uma consulta para obter informações sobre os pedidos, incluindo o nome do cliente, a data do pedido, o valor total e os itens associados a cada pedido, incluindo o nome do produto, a quantidade e o preço unitário.