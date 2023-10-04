Claro! Aqui está um código complexo em SQL que cria uma tabela para armazenar informações de funcionários em uma empresa fictícia:

```
-- Criação da tabela Funcionario
CREATE TABLE Funcionario (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    cargo VARCHAR(50),
    salario DECIMAL(10, 2),
    departamento_id INT,
    data_contratacao DATE,
    endereco VARCHAR(100)
);

-- Criação da tabela Departamento
CREATE TABLE Departamento (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    localizacao VARCHAR(100)
);

-- Inserção de informações na tabela Departamento
INSERT INTO Departamento (id, nome, localizacao)
VALUES 
    (1, 'Vendas', 'São Paulo'),
    (2, 'Marketing', 'Rio de Janeiro'),
    (3, 'RH', 'Belo Horizonte');

-- Inserção de informações na tabela Funcionario
INSERT INTO Funcionario (id, nome, cargo, salario, departamento_id, data_contratacao, endereco)
VALUES 
    (1, 'João Silva', 'Vendedor', 3000.00, 1, '2020-01-15', 'Rua A, 123'),
    (2, 'Maria Souza', 'Analista de Marketing', 4000.00, 2, '2019-05-10', 'Avenida B, 456'),
    (3, 'Pedro Santos', 'Analista de RH', 3500.00, 3, '2021-03-20', 'Rua C, 789');

-- Consulta para obter todos os funcionários e seus respectivos departamentos
SELECT 
    f.nome AS 'Nome do Funcionário',
    f.cargo AS 'Cargo',
    f.salario AS 'Salário',
    d.nome AS 'Departamento',
    d.localizacao AS 'Localização'
FROM 
    Funcionario f
INNER JOIN 
    Departamento d ON f.departamento_id = d.id;
```

Nesse código, começamos criando duas tabelas: "Funcionario" e "Departamento". A tabela "Funcionario" possui colunas para o ID, nome, cargo, salário, ID do departamento, data de contratação e endereço do funcionário. A tabela "Departamento" possui colunas para o ID, nome e localização do departamento.

Em seguida, inserimos algumas informações nas tabelas para exemplificar o funcionamento. Na tabela "Departamento", inserimos três registros fictícios. Na tabela "Funcionario", inserimos três registros fictícios, relacionando cada funcionário a um departamento.

Por fim, realizamos uma consulta que retorna o nome do funcionário, cargo, salário, nome do departamento e localização para todos os funcionários. Utilizamos a cláusula "JOIN" para relacionar as duas tabelas, utilizando o ID do departamento como chave de ligação.

Esse é apenas um exemplo de código complexo em SQL, mas é importante ressaltar que a complexidade pode variar dependendo das necessidades do projeto.