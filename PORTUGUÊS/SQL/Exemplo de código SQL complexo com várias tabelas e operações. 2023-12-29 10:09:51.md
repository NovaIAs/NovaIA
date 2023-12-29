Claro! Aqui está um exemplo de código SQL complexo que envolve várias tabelas e operações:

```
-- Criação das tabelas
CREATE TABLE clientes (
  id_cliente INT PRIMARY KEY,
  nome VARCHAR(50),
  email VARCHAR(50),
  telefone VARCHAR(15)
);

CREATE TABLE pedidos (
  id_pedido INT PRIMARY KEY,
  id_cliente INT,
  data_pedido DATE,
  valor_total DECIMAL(10, 2),
  FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE itens_pedido (
  id_item INT PRIMARY KEY,
  id_pedido INT,
  id_produto INT,
  quantidade INT,
  valor_unitario DECIMAL(10, 2),
  FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
  FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

CREATE TABLE produtos (
  id_produto INT PRIMARY KEY,
  nome VARCHAR(50),
  preco DECIMAL(10, 2)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id_cliente, nome, email, telefone)
VALUES (1, 'João Silva', 'joao.silva@email.com', '(11) 99999-9999');

INSERT INTO produtos (id_produto, nome, preco)
VALUES (1, 'Camiseta', 39.99),
       (2, 'Calça', 79.99),
       (3, 'Tênis', 129.99);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido, valor_total)
VALUES (1, 1, '2021-01-01', 169.97);

INSERT INTO itens_pedido (id_item, id_pedido, id_produto, quantidade, valor_unitario)
VALUES (1, 1, 1, 2, 39.99),
       (2, 1, 2, 1, 79.99),
       (3, 1, 3, 1, 129.99);

-- Consulta de pedidos e seus itens
SELECT p.id_pedido, c.nome AS cliente, p.data_pedido, p.valor_total,
       ip.quantidade, pr.nome AS produto, ip.valor_unitario,
       (ip.valor_unitario * ip.quantidade) AS subtotal
FROM pedidos p
JOIN clientes c ON p.id_cliente = c.id_cliente
JOIN itens_pedido ip ON p.id_pedido = ip.id_pedido
JOIN produtos pr ON ip.id_produto = pr.id_produto
WHERE p.id_pedido = 1;
```

Este código SQL cria um conjunto de tabelas relacionais para armazenar informações de clientes, pedidos, itens de pedido e produtos. Em seguida, insere alguns dados nas tabelas e realiza uma consulta para recuperar informações específicas de um pedido, incluindo detalhes dos itens do pedido.

A consulta usa as cláusulas JOIN para combinar as tabelas relacionadas e retorna uma linha para cada item do pedido, mostrando o ID do pedido, o nome do cliente, a data do pedido, o valor total do pedido, a quantidade de cada item, o nome do produto, o valor unitário do produto e o subtotal (quantidade * valor unitário) de cada item do pedido.

Espero que este exemplo atenda às suas expectativas! Lembre-se de adaptar os dados e as consultas de acordo com suas necessidades específicas.