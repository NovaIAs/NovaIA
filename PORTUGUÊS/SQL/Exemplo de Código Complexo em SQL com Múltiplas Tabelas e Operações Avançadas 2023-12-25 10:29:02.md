Com certeza! Aqui está um exemplo de um código complexo em SQL que envolve várias tabelas e operações avançadas:

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS Cliente (
    id_cliente INT PRIMARY KEY,
    nome_cliente VARCHAR(100),
    endereco_cliente VARCHAR(200)
);

CREATE TABLE IF NOT EXISTS Pedido (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    FOREIGN KEY (id_cliente) REFERENCES Cliente(id_cliente)
);

CREATE TABLE IF NOT EXISTS ItemPedido (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    FOREIGN KEY (id_pedido) REFERENCES Pedido(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES Produto(id_produto)
);

CREATE TABLE IF NOT EXISTS Produto (
    id_produto INT PRIMARY KEY,
    nome_produto VARCHAR(100),
    preco FLOAT
);

-- Inserção de dados
INSERT INTO Cliente (id_cliente, nome_cliente, endereco_cliente)
VALUES (1, 'João da Silva', 'Rua A, 123'),
       (2, 'Maria Souza', 'Avenida B, 456'),
       (3, 'Pedro Santos', 'Travessa C, 789');

INSERT INTO Produto (id_produto, nome_produto, preco)
VALUES (1, 'Camiseta', 29.90),
       (2, 'Calça Jeans', 89.90),
       (3, 'Tênis', 119.90);

INSERT INTO Pedido (id_pedido, id_cliente, data_pedido)
VALUES (1, 1, '2021-01-01'),
       (2, 2, '2021-02-05'),
       (3, 3, '2021-03-10');

INSERT INTO ItemPedido (id_item, id_pedido, id_produto, quantidade)
VALUES (1, 1, 1, 2),
       (2, 1, 2, 1),
       (3, 2, 3, 1),
       (4, 3, 1, 3),
       (5, 3, 2, 2),
       (6, 3, 3, 1);

-- Consulta de dados
SELECT c.nome_cliente, p.nome_produto, ip.quantidade, (p.preco * ip.quantidade) AS valor_total
FROM Cliente c
INNER JOIN Pedido pd ON c.id_cliente = pd.id_cliente
INNER JOIN ItemPedido ip ON pd.id_pedido = ip.id_pedido
INNER JOIN Produto p ON ip.id_produto = p.id_produto
WHERE pd.data_pedido BETWEEN '2021-01-01' AND '2021-12-31';
```

Neste exemplo, temos a criação de quatro tabelas: Cliente, Pedido, ItemPedido e Produto. A tabela Cliente armazena informações básicas dos clientes, como ID, nome e endereço. A tabela Pedido guarda informações sobre os pedidos, como ID, ID do cliente e data do pedido. A tabela ItemPedido relaciona os pedidos com os produtos, armazenando o ID do item, o ID do pedido, o ID do produto e a quantidade. A tabela Produto armazena informações sobre os produtos, como ID, nome e preço.

Em seguida, são realizadas inserções de dados nas tabelas Cliente, Produto e Pedido, preenchendo-as com informações fictícias.

Por fim, é feita uma consulta que retorna o nome do cliente, o nome do produto, a quantidade e o valor total de cada item do pedido, considerando apenas os pedidos feitos entre 01 de janeiro e 31 de dezembro de 2021. A consulta utiliza os comandos INNER JOIN para relacionar as tabelas e calcular o valor total multiplicando o preço do produto pela quantidade.