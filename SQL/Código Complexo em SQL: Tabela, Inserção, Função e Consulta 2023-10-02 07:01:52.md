Claro! Aqui está um exemplo de um código complexo em SQL que envolve a criação de uma tabela, inserção de dados, criação de uma função e consulta avançada:

```sql
-- Criação da tabela
CREATE TABLE Clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    endereco VARCHAR(100),
    cidade VARCHAR(50),
    estado VARCHAR(2)
);

-- Inserção de dados
INSERT INTO Clientes (id, nome, endereco, cidade, estado)
VALUES (1, 'João', 'Rua A, 123', 'São Paulo', 'SP'),
       (2, 'Maria', 'Avenida B, 456', 'Rio de Janeiro', 'RJ'),
       (3, 'Pedro', 'Travessa C, 789', 'Belo Horizonte', 'MG');

-- Criação de uma função para contar o número de clientes por estado
CREATE FUNCTION ContarClientesPorEstado(estado VARCHAR(2)) RETURNS INT
BEGIN
    DECLARE quantidade INT;
    SET quantidade = (SELECT COUNT(*) FROM Clientes WHERE estado = estado);
    RETURN quantidade;
END;

-- Consulta avançada utilizando a função criada
SELECT estado, ContarClientesPorEstado(estado) AS quantidade_clientes
FROM Clientes
GROUP BY estado;
```

Espero que esse código atenda às suas expectativas! Ele cria uma tabela chamada "Clientes" com colunas para id, nome, endereço, cidade e estado. Em seguida, insere alguns dados nessa tabela. Depois, é criada uma função chamada "ContarClientesPorEstado" que conta o número de clientes por estado. Por fim, é feita uma consulta que utiliza essa função para mostrar a quantidade de clientes por estado.