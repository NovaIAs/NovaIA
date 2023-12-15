Claro! Aqui está um código complexo em SQL que aborda uma situação de gerenciamento de estoque em um sistema de e-commerce:

```
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS produtos (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    descricao TEXT,
    preco DECIMAL(10,2),
    estoque INT
);

CREATE TABLE IF NOT EXISTS pedidos (
    id INT PRIMARY KEY,
    data_pedido DATE,
    status VARCHAR(50),
    total DECIMAL(10,2)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES pedidos(id),
    FOREIGN KEY (produto_id) REFERENCES produtos(id)
);

-- Função para atualizar o estoque
CREATE OR REPLACE FUNCTION atualizar_estoque(p_pedido_id INT)
RETURNS VOID AS $$
DECLARE
    v_item RECORD;
BEGIN
    FOR v_item IN 
        SELECT ip.produto_id, ip.quantidade
        FROM itens_pedido ip
        WHERE ip.pedido_id = p_pedido_id
    LOOP
        UPDATE produtos
        SET estoque = estoque - v_item.quantidade
        WHERE id = v_item.produto_id;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

-- Gatilho para atualizar o estoque quando um pedido for inserido
CREATE TRIGGER atualizar_estoque_trigger
AFTER INSERT ON pedidos
FOR EACH ROW
EXECUTE FUNCTION atualizar_estoque(NEW.id);

-- Gatilho para atualizar o estoque quando um pedido for atualizado
CREATE TRIGGER atualizar_estoque_trigger
AFTER UPDATE ON pedidos
FOR EACH ROW
EXECUTE FUNCTION atualizar_estoque(NEW.id);

-- Gatilho para atualizar o estoque quando um pedido for excluído
CREATE TRIGGER atualizar_estoque_trigger
AFTER DELETE ON pedidos
FOR EACH ROW
EXECUTE FUNCTION atualizar_estoque(OLD.id);
```

Neste código, estamos criando três tabelas: "produtos", "pedidos" e "itens_pedido". A tabela "produtos" armazena informações sobre os produtos, como nome, descrição, preço e estoque. A tabela "pedidos" registra os pedidos feitos pelos clientes, contendo informações como a data do pedido, o status e o valor total. A tabela "itens_pedido" serve para relacionar os produtos aos pedidos, armazenando a quantidade de cada produto em cada pedido.

Em seguida, criamos uma função chamada "atualizar_estoque". Essa função é responsável por atualizar o estoque dos produtos sempre que um pedido é inserido, atualizado ou excluído. Ela percorre os itens do pedido e subtrai as quantidades correspondentes do estoque dos produtos relacionados.

Por fim, criamos três gatilhos (triggers) que acionam a função "atualizar_estoque" quando ocorrem determinados eventos nos pedidos: inserção, atualização e exclusão. Isso garante que o estoque seja atualizado automaticamente sempre que houver uma alteração nos pedidos.

Esse código é um exemplo de como podemos utilizar SQL para gerenciar o estoque de um sistema de e-commerce de forma automatizada.