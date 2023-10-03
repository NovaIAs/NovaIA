Claro! Aqui está um código complexo em SQL que envolve várias tabelas e funcionalidades avançadas. Ele foi projetado para um sistema de gerenciamento de vendas em uma loja online.

```sql
-- Criação das tabelas

CREATE TABLE IF NOT EXISTS clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(50),
    email VARCHAR(50),
    telefone VARCHAR(15)
);

CREATE TABLE IF NOT EXISTS produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(50),
    preco DECIMAL(10, 2),
    estoque INT
);

CREATE TABLE IF NOT EXISTS pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    total DECIMAL(10, 2),
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

-- Inserção de dados

INSERT INTO clientes (id_cliente, nome, email, telefone)
VALUES (1, 'João Silva', 'joao@email.com', '1234567890');

INSERT INTO produtos (id_produto, nome, preco, estoque)
VALUES (1, 'Camiseta', 29.90, 10),
       (2, 'Calça Jeans', 99.90, 5),
       (3, 'Tênis', 149.90, 8);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido, total)
VALUES (1, 1, '2022-01-01', 129.80);

INSERT INTO itens_pedido (id_pedido, id_produto, quantidade)
VALUES (1, 1, 2),
       (1, 3, 1);

-- Consultas

-- Consulta para obter todos os pedidos de um cliente específico, incluindo o nome do cliente e o total de cada pedido
SELECT p.id_pedido, c.nome AS nome_cliente, p.total
FROM pedidos p
INNER JOIN clientes c ON c.id_cliente = p.id_cliente
WHERE c.id_cliente = 1;

-- Consulta para obter o estoque atual de um produto específico
SELECT estoque
FROM produtos
WHERE id_produto = 1;

-- Atualização do estoque de um produto após uma venda
UPDATE produtos
SET estoque = estoque - 1
WHERE id_produto = 1;
```

Este código SQL cria quatro tabelas: `clientes`, `produtos`, `pedidos` e `itens_pedido`. A tabela `clientes` armazena informações sobre os clientes, como nome, email e telefone. A tabela `produtos` armazena informações sobre os produtos disponíveis para venda, como nome, preço e quantidade em estoque. A tabela `pedidos` registra os pedidos feitos pelos clientes, incluindo a data do pedido e o valor total. A tabela `itens_pedido` associa os produtos aos pedidos, incluindo a quantidade de cada produto.

O código também inclui instruções para inserir dados nessas tabelas, como a criação de clientes, produtos, pedidos e itens de pedido. Além disso, há consultas para obter informações específicas, como todos os pedidos de um cliente específico, o estoque atual de um produto e atualização do estoque após uma venda.

Observe que o código é apenas um exemplo e pode não ser adequado para uma implementação real. É importante adaptá-lo às necessidades do sistema específico.