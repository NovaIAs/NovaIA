Claro! Aqui está um exemplo de código SQL complexo que realiza uma análise de dados de uma empresa fictícia:

```sql
-- Criação da tabela de funcionários
CREATE TABLE funcionarios (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    cargo VARCHAR(50),
    salario DECIMAL(10, 2),
    departamento_id INT
);

-- Inserção de dados na tabela de funcionários
INSERT INTO funcionarios (id, nome, cargo, salario, departamento_id)
VALUES 
    (1, 'João', 'Gerente', 5000.00, 1),
    (2, 'Maria', 'Analista', 3000.00, 2),
    (3, 'Pedro', 'Desenvolvedor', 4000.00, 2),
    (4, 'Ana', 'Analista', 3200.00, 1),
    (5, 'Carlos', 'Estagiário', 1500.00, 3),
    (6, 'Laura', 'Analista', 3500.00, 1),
    (7, 'Paulo', 'Desenvolvedor', 4200.00, 2),
    (8, 'Mariana', 'Gerente', 5500.00, 1),
    (9, 'Lucas', 'Desenvolvedor', 3800.00, 2),
    (10, 'Larissa', 'Analista', 3300.00, 3);

-- Criação da tabela de departamentos
CREATE TABLE departamentos (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    localizacao VARCHAR(100)
);

-- Inserção de dados na tabela de departamentos
INSERT INTO departamentos (id, nome, localizacao)
VALUES 
    (1, 'Vendas', 'São Paulo'),
    (2, 'TI', 'Rio de Janeiro'),
    (3, 'RH', 'Belo Horizonte');

-- Seleção dos funcionários com salário acima da média do departamento
SELECT f.nome, d.nome as departamento, f.salario
FROM funcionarios f
INNER JOIN departamentos d ON f.departamento_id = d.id
WHERE f.salario > (
    SELECT AVG(salario)
    FROM funcionarios
    WHERE departamento_id = f.departamento_id
)
ORDER BY departamento, salario DESC;
```

Este exemplo cria duas tabelas: "funcionarios" e "departamentos". A tabela "funcionarios" armazena informações dos funcionários, como id, nome, cargo, salário e departamento_id. A tabela "departamentos" armazena informações dos departamentos, como id, nome e localização.

Em seguida, são inseridos dados nas tabelas utilizando a cláusula INSERT INTO.

O código final realiza uma seleção dos funcionários cujo salário é maior do que a média salarial do departamento em que estão. Isso é feito através de um JOIN entre as tabelas "funcionarios" e "departamentos" e uma subconsulta que calcula a média salarial do departamento. A saída é ordenada pelo departamento e pelo salário em ordem decrescente.

Espero que este exemplo de código SQL atenda às suas expectativas! Caso tenha alguma dúvida, estou à disposição para ajudar.