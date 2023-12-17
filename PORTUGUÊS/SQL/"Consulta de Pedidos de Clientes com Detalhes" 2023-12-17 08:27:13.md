Claro! Aqui está um código SQL complexo que realiza uma série de operações em um banco de dados de uma loja de produtos. O código é dividido em várias partes para facilitar o entendimento:

1. Primeiro, vamos criar as tabelas necessárias:

```sql
CREATE TABLE clientes (
  id_cliente INT PRIMARY KEY,
  nome VARCHAR(100),
  email VARCHAR(100),
  endereco VARCHAR(200)
);

CREATE TABLE produtos (
  id_produto INT PRIMARY KEY,
  nome VARCHAR(100),
  preco DECIMAL(10, 2),
  quantidade INT
);

CREATE TABLE pedidos (
  id_pedido INT PRIMARY KEY,
  id_cliente INT,
  data_pedido DATE,
  FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE itens_pedido (
  id_pedido INT,
  id_produto INT,
  quantidade INT,
  FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
  FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);
```

2. Em seguida, vamos inserir alguns registros nas tabelas:

```sql
INSERT INTO clientes (id_cliente, nome, email, endereco) VALUES
  (1, 'João da Silva', 'joao@email.com', 'Rua A, 123'),
  (2, 'Maria Souza', 'maria@email.com', 'Av. B, 456');

INSERT INTO produtos (id_produto, nome, preco, quantidade) VALUES
  (1, 'Camiseta', 29.90, 100),
  (2, 'Calça', 59.90, 50),
  (3, 'Tênis', 99.90, 20);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido) VALUES
  (1, 1, '2022-01-01'),
  (2, 2, '2022-01-02');

INSERT INTO itens_pedido (id_pedido, id_produto, quantidade) VALUES
  (1, 1, 2),
  (1, 3, 1),
  (2, 2, 3);
```

3. Agora, vamos fazer uma consulta para obter informações sobre os pedidos feitos pelos clientes:

```sql
SELECT 
  p.id_pedido,
  c.nome AS nome_cliente,
  c.endereco AS endereco_cliente,
  pr.nome AS nome_produto,
  i.quantidade,
  pr.preco * i.quantidade AS total
FROM
  pedidos p
  JOIN clientes c ON p.id_cliente = c.id_cliente
  JOIN itens_pedido i ON p.id_pedido = i.id_pedido
  JOIN produtos pr ON i.id_produto = pr.id_produto
WHERE
  p.data_pedido >= '2022-01-01'
ORDER BY
  p.id_pedido;
```

O código acima realiza um JOIN entre as tabelas `pedidos`, `clientes`, `itens_pedido` e `produtos` para obter informações sobre os pedidos feitos pelos clientes. Ele seleciona o ID do pedido, o nome do cliente, o endereço do cliente, o nome do produto, a quantidade do produto e o total (preço do produto multiplicado pela quantidade). A consulta retorna apenas os pedidos feitos a partir de 01 de janeiro de 2022 e ordena os resultados pelo ID do pedido.