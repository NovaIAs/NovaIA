Claro! Aqui está um exemplo de um código complexo em SQL:

```sql
-- Criação de Tabelas
CREATE TABLE IF NOT EXISTS clientes (
  id_cliente INT PRIMARY KEY,
  nome VARCHAR(100),
  email VARCHAR(100),
  telefone VARCHAR(15)
);

CREATE TABLE IF NOT EXISTS pedidos (
  id_pedido INT PRIMARY KEY,
  id_cliente INT,
  data_pedido DATE,
  valor_total DECIMAL(10, 2),
  FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS produtos (
  id_produto INT PRIMARY KEY,
  nome VARCHAR(100),
  preco DECIMAL(10, 2)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
  id_item INT PRIMARY KEY,
  id_pedido INT,
  id_produto INT,
  quantidade INT,
  FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
  FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

-- Inserção de Dados
INSERT INTO clientes (id_cliente, nome, email, telefone) VALUES
  (1, 'João Silva', 'joao.silva@example.com', '(99) 99999-9999'),
  (2, 'Maria Santos', 'maria.santos@example.com', '(88) 88888-8888'),
  (3, 'Pedro Almeida', 'pedro.almeida@example.com', '(77) 77777-7777');

INSERT INTO produtos (id_produto, nome, preco) VALUES
  (1, 'Camisa', 49.99),
  (2, 'Calça', 89.99),
  (3, 'Tênis', 129.99);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido, valor_total) VALUES
  (1, 1, '2022-01-01', 139.98),
  (2, 2, '2022-02-01', 89.99),
  (3, 3, '2022-03-01', 219.98);

INSERT INTO itens_pedido (id_item, id_pedido, id_produto, quantidade) VALUES
  (1, 1, 1, 2),
  (2, 1, 2, 1),
  (3, 2, 3, 1),
  (4, 3, 1, 1),
  (5, 3, 2, 1),
  (6, 3, 3, 1);

-- Consultas
-- Consulta de todos os clientes
SELECT * FROM clientes;

-- Consulta de pedidos de um cliente específico
SELECT p.id_pedido, p.data_pedido, p.valor_total, pr.nome, pr.preco, ip.quantidade
FROM pedidos p
JOIN itens_pedido ip ON p.id_pedido = ip.id_pedido
JOIN produtos pr ON ip.id_produto = pr.id_produto
WHERE p.id_cliente = 1;

-- Consulta de produtos mais vendidos
SELECT pr.nome, SUM(ip.quantidade) AS total_vendido
FROM itens_pedido ip
JOIN produtos pr ON ip.id_produto = pr.id_produto
GROUP BY pr.nome
ORDER BY total_vendido DESC;
```

Este código SQL cria e popula uma estrutura de banco de dados com 4 tabelas: `clientes`, `pedidos`, `produtos` e `itens_pedido`. Em seguida, realiza consultas para obter informações sobre os clientes, pedidos e produtos.

A tabela `clientes` armazena informações sobre os clientes, como nome, e-mail e telefone. A tabela `pedidos` registra os pedidos feitos pelos clientes, com data, valor total e uma chave estrangeira para o cliente correspondente. A tabela `produtos` contém informações sobre os produtos disponíveis, como nome e preço. A tabela `itens_pedido` associa os produtos aos pedidos, registrando a quantidade de cada produto em um determinado pedido.

As consultas realizadas no código incluem uma consulta para obter todos os clientes, uma consulta para obter os pedidos de um cliente específico e uma consulta para obter os produtos mais vendidos.

Espero que este código complexo em SQL atenda às suas expectativas!