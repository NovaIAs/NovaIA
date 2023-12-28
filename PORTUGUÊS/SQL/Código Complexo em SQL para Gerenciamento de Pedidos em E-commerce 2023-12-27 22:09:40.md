Claro! Aqui está um código complexo em SQL que envolve várias tabelas, funções e procedimentos armazenados. Ele é projetado para simular uma aplicação de gerenciamento de pedidos em um e-commerce.

```sql
-- Criação das tabelas
CREATE TABLE clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200),
    email VARCHAR(100)
);

CREATE TABLE produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10,2),
    estoque INT
);

CREATE TABLE pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    status VARCHAR(50),
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE itens_pedido (
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    PRIMARY KEY (id_pedido, id_produto),
    FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

-- Criação das funções
DELIMITER //

CREATE FUNCTION calcular_total_pedido(id_pedido INT) RETURNS DECIMAL(10,2)
BEGIN
    DECLARE total DECIMAL(10,2);
    
    SELECT SUM(p.preco * ip.quantidade)
    INTO total
    FROM produtos p
    INNER JOIN itens_pedido ip ON p.id_produto = ip.id_produto
    WHERE ip.id_pedido = id_pedido;
    
    RETURN total;
END //

DELIMITER ;

-- Criação dos procedimentos armazenados
DELIMITER //

CREATE PROCEDURE criar_pedido(
    IN id_cliente INT,
    IN produtos_pedido VARCHAR(255)
)
BEGIN
    DECLARE id_pedido INT;
    
    INSERT INTO pedidos (id_cliente, data_pedido, status)
    VALUES (id_cliente, CURRENT_DATE, 'Em andamento');
    
    SET id_pedido = LAST_INSERT_ID();
    
    INSERT INTO itens_pedido (id_pedido, id_produto, quantidade)
    SELECT id_pedido, id_produto, quantidade
    FROM (
        SELECT id_produto, quantidade
        FROM produtos
        WHERE id_produto IN (SELECT CAST(value AS INT) FROM STRING_SPLIT(produtos_pedido, ',')) -- SQL Server
        -- WHERE id_produto IN (SELECT CAST(token AS INT) FROM STRTOK_SPLIT_TO_TABLE(produtos_pedido, ',')) -- PostgreSQL
    ) AS p;
    
    UPDATE produtos p
    INNER JOIN itens_pedido ip ON p.id_produto = ip.id_produto
    SET p.estoque = p.estoque - ip.quantidade
    WHERE ip.id_pedido = id_pedido;
END //

DELIMITER ;

-- Exemplo de utilização do procedimento
CALL criar_pedido(1, '1,2,3');
```

Neste código SQL, criamos quatro tabelas: `clientes`, `produtos`, `pedidos` e `itens_pedido`. A tabela `clientes` armazena informações sobre os clientes, como nome, endereço e e-mail. A tabela `produtos` armazena informações sobre os produtos disponíveis para venda, como nome, preço e estoque. A tabela `pedidos` armazena informações sobre os pedidos feitos pelos clientes, como o ID do cliente, data do pedido e status. A tabela `itens_pedido` armazena os itens de cada pedido, relacionando o ID do pedido com o ID do produto e a quantidade solicitada.

Além das tabelas, o código também cria uma função chamada `calcular_total_pedido`, que recebe o ID de um pedido como parâmetro e retorna o valor total do pedido, calculado somando o preço de cada produto multiplicado pela quantidade solicitada.

O código também inclui um procedimento armazenado chamado `criar_pedido`, que cria um novo pedido para um determinado cliente e registra os itens selecionados no pedido. O procedimento recebe o ID do cliente e uma string contendo os IDs dos produtos selecionados, separados por vírgula. Os produtos são inseridos na tabela `itens_pedido`, e o estoque de cada produto é atualizado.

Por fim, há um exemplo de utilização do procedimento, chamando-o com o ID do cliente e uma string de IDs de produtos.

Esse código é apenas um exemplo de como um sistema de gerenciamento de pedidos em um e-commerce pode ser implementado em SQL. Ele pode ser adaptado e expandido de acordo com as necessidades específicas do projeto.