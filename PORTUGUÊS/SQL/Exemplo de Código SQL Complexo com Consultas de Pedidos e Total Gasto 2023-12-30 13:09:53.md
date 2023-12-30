Certamente! Aqui está um exemplo de código SQL complexo que envolve várias tabelas, funções e operações, com comentários explicativos em cada seção:

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS clientes (
  id INT PRIMARY KEY,
  nome VARCHAR(100),
  endereco VARCHAR(200)
);

CREATE TABLE IF NOT EXISTS pedidos (
  id INT PRIMARY KEY,
  cliente_id INT,
  data_pedido DATE,
  FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
  id INT PRIMARY KEY,
  pedido_id INT,
  produto VARCHAR(100),
  quantidade INT,
  preco DECIMAL(10, 2),
  FOREIGN KEY (pedido_id) REFERENCES pedidos(id)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id, nome, endereco)
VALUES (1, 'João Silva', 'Rua A, 123'),
       (2, 'Maria Santos', 'Av. B, 456');

INSERT INTO pedidos (id, cliente_id, data_pedido)
VALUES (1, 1, '2022-01-01'),
       (2, 2, '2022-01-02');

INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, preco)
VALUES (1, 1, 'Camiseta', 2, 50.00),
       (2, 1, 'Calça', 1, 80.00),
       (3, 2, 'Tênis', 1, 120.00);

-- Consulta para obter todos os pedidos de um cliente específico
SELECT c.nome AS 'Nome do Cliente', p.data_pedido AS 'Data do Pedido', i.produto AS 'Produto', i.quantidade AS 'Quantidade', i.preco AS 'Preço'
FROM clientes c
JOIN pedidos p ON c.id = p.cliente_id
JOIN itens_pedido i ON p.id = i.pedido_id
WHERE c.id = 1;

-- Consulta para obter o total gasto por cada cliente
SELECT c.nome AS 'Nome do Cliente', SUM(i.quantidade * i.preco) AS 'Total Gasto'
FROM clientes c
JOIN pedidos p ON c.id = p.cliente_id
JOIN itens_pedido i ON p.id = i.pedido_id
GROUP BY c.nome;
```

Este código SQL cria três tabelas: `clientes`, `pedidos` e `itens_pedido`. Em seguida, insere dados nessas tabelas e realiza duas consultas. 

A primeira consulta obtém todos os pedidos de um cliente específico (no exemplo, o cliente com `id` igual a 1), exibindo o nome do cliente, a data do pedido, o produto, a quantidade e o preço.

A segunda consulta calcula o total gasto por cada cliente, somando a quantidade de cada item de pedido multiplicada pelo seu preço. A consulta agrupa os resultados pelo nome do cliente.