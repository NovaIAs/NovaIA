Claro! Aqui está um exemplo de um código complexo em SQL para criar um sistema de gerenciamento de vendas de uma loja com múltiplas tabelas:

```
-- Criação da tabela "clientes" para armazenar informações sobre os clientes
CREATE TABLE clientes (
  id_cliente INT PRIMARY KEY,
  nome VARCHAR(100),
  endereco VARCHAR(200),
  telefone VARCHAR(20)
);

-- Criação da tabela "produtos" para armazenar informações sobre os produtos disponíveis na loja
CREATE TABLE produtos (
  id_produto INT PRIMARY KEY,
  nome VARCHAR(100),
  descricao VARCHAR(500),
  preco DECIMAL(10, 2)
);

-- Criação da tabela "vendas" para armazenar informações sobre as vendas realizadas
CREATE TABLE vendas (
  id_venda INT PRIMARY KEY,
  data_venda DATE,
  id_cliente INT,
  FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

-- Criação da tabela "itens_venda" para armazenar informações sobre os produtos vendidos em cada venda
CREATE TABLE itens_venda (
  id_item INT PRIMARY KEY,
  id_venda INT,
  id_produto INT,
  quantidade INT,
  preco_unitario DECIMAL(10, 2),
  FOREIGN KEY (id_venda) REFERENCES vendas(id_venda),
  FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

-- Inserção de dados na tabela "clientes"
INSERT INTO clientes (id_cliente, nome, endereco, telefone) VALUES
  (1, 'João Silva', 'Rua A, 123', '9999-1234'),
  (2, 'Maria Santos', 'Avenida B, 456', '8888-5678'),
  (3, 'Pedro Oliveira', 'Travessa C, 789', '7777-9876');

-- Inserção de dados na tabela "produtos"
INSERT INTO produtos (id_produto, nome, descricao, preco) VALUES
  (1, 'Camiseta', 'Camiseta de algodão', 29.90),
  (2, 'Calça', 'Calça jeans', 79.90),
  (3, 'Tênis', 'Tênis esportivo', 99.90);

-- Inserção de dados na tabela "vendas"
INSERT INTO vendas (id_venda, data_venda, id_cliente) VALUES
  (1, '2022-01-01', 1),
  (2, '2022-01-02', 2),
  (3, '2022-01-03', 3);

-- Inserção de dados na tabela "itens_venda"
INSERT INTO itens_venda (id_item, id_venda, id_produto, quantidade, preco_unitario) VALUES
  (1, 1, 1, 2, 29.90),
  (2, 1, 2, 1, 79.90),
  (3, 2, 3, 1, 99.90),
  (4, 3, 1, 3, 29.90),
  (5, 3, 2, 2, 79.90);

-- Consulta para obter todas as vendas realizadas por um cliente específico
SELECT v.id_venda, v.data_venda, c.nome AS nome_cliente, p.nome AS nome_produto, iv.quantidade, iv.preco_unitario
FROM vendas v
JOIN clientes c ON v.id_cliente = c.id_cliente
JOIN itens_venda iv ON v.id_venda = iv.id_venda
JOIN produtos p ON iv.id_produto = p.id_produto
WHERE c.id_cliente = 1;
```

Este código cria quatro tabelas: "clientes", "produtos", "vendas" e "itens_venda". Cada tabela tem sua estrutura definida com suas respectivas colunas e chaves primárias e estrangeiras.

Em seguida, são inseridos alguns dados de exemplo nas tabelas "clientes", "produtos", "vendas" e "itens_venda" utilizando a cláusula `INSERT INTO`.

Por fim, é realizada uma consulta para obter todas as vendas realizadas por um cliente específico (neste caso, o cliente com `id_cliente` igual a 1), mostrando as informações da venda, nome do cliente, nome do produto, quantidade e preço unitário.

Espero que este exemplo atenda às suas expectativas!