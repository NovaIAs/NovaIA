Claro! Aqui está um exemplo de um código complexo em SQL que envolve múltiplas tabelas e utiliza várias funcionalidades avançadas:

```
-- Criação das tabelas
CREATE TABLE Cliente (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200)
);

CREATE TABLE Pedido (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES Cliente(id)
);

CREATE TABLE Produto (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2)
);

CREATE TABLE ItemPedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES Pedido(id),
    FOREIGN KEY (produto_id) REFERENCES Produto(id)
);

-- Inserção de dados nas tabelas
INSERT INTO Cliente (id, nome, endereco)
VALUES (1, 'João Silva', 'Rua A, 123'),
       (2, 'Maria Santos', 'Avenida B, 456');

INSERT INTO Produto (id, nome, preco)
VALUES (1, 'Camiseta', 29.99),
       (2, 'Calça Jeans', 79.99),
       (3, 'Tênis', 99.99);

INSERT INTO Pedido (id, cliente_id, data_pedido, valor_total)
VALUES (1, 1, '2022-01-01', 129.98),
       (2, 2, '2022-02-01', 209.97);

INSERT INTO ItemPedido (id, pedido_id, produto_id, quantidade)
VALUES (1, 1, 1, 2),
       (2, 1, 2, 1),
       (3, 2, 1, 3),
       (4, 2, 3, 2);

-- Consulta complexa
SELECT c.nome AS cliente,
       p.data_pedido AS data,
       SUM(ip.quantidade * pr.preco) AS valor_total
FROM Cliente c
JOIN Pedido p ON c.id = p.cliente_id
JOIN ItemPedido ip ON p.id = ip.pedido_id
JOIN Produto pr ON ip.produto_id = pr.id
GROUP BY c.nome, p.data_pedido
HAVING SUM(ip.quantidade * pr.preco) > 100;

```

Neste código SQL, criamos quatro tabelas: Cliente, Pedido, Produto e ItemPedido. As tabelas Cliente e Produto armazenam informações sobre os clientes e produtos, respectivamente. A tabela Pedido registra informações sobre os pedidos feitos pelos clientes, como a data do pedido e o valor total. A tabela ItemPedido faz a associação entre os pedidos e os produtos, incluindo a quantidade de cada produto em um determinado pedido.

Em seguida, inserimos alguns dados de exemplo nas tabelas usando comandos INSERT INTO.

Por fim, realizamos uma consulta complexa que retorna o nome do cliente, a data do pedido e o valor total para todos os pedidos cujo valor total seja superior a 100. A consulta utiliza as cláusulas JOIN para relacionar as tabelas e a cláusula GROUP BY para agrupar os resultados por cliente e data do pedido. A cláusula HAVING é usada para filtrar apenas os pedidos cujo valor total seja maior que 100.