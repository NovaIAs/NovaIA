Claro! Aqui está um exemplo de código SQL complexo que envolve várias tabelas e operações:

```
-- Criação das tabelas
CREATE TABLE departamento (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    localizacao VARCHAR(100)
);

CREATE TABLE funcionario (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    salario DECIMAL(10, 2),
    departamento_id INT,
    FOREIGN KEY (departamento_id) REFERENCES departamento(id)
);

CREATE TABLE projeto (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    descricao TEXT,
    departamento_id INT,
    FOREIGN KEY (departamento_id) REFERENCES departamento(id)
);

CREATE TABLE tarefa (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    descricao TEXT,
    data_inicio DATE,
    data_fim DATE,
    projeto_id INT,
    funcionario_id INT,
    FOREIGN KEY (projeto_id) REFERENCES projeto(id),
    FOREIGN KEY (funcionario_id) REFERENCES funcionario(id)
);

-- Inserção de dados nas tabelas
INSERT INTO departamento (id, nome, localizacao) VALUES
    (1, 'Departamento A', 'São Paulo'),
    (2, 'Departamento B', 'Rio de Janeiro'),
    (3, 'Departamento C', 'Brasília');

INSERT INTO funcionario (id, nome, salario, departamento_id) VALUES
    (1, 'João', 5000.00, 1),
    (2, 'Maria', 6000.00, 1),
    (3, 'Pedro', 4500.00, 2),
    (4, 'Ana', 5500.00, 2),
    (5, 'José', 4000.00, 3);

INSERT INTO projeto (id, nome, descricao, departamento_id) VALUES
    (1, 'Projeto X', 'Descrição do Projeto X', 1),
    (2, 'Projeto Y', 'Descrição do Projeto Y', 2),
    (3, 'Projeto Z', 'Descrição do Projeto Z', 3);

INSERT INTO tarefa (id, nome, descricao, data_inicio, data_fim, projeto_id, funcionario_id) VALUES
    (1, 'Tarefa 1', 'Descrição da Tarefa 1', '2022-01-01', '2022-01-10', 1, 1),
    (2, 'Tarefa 2', 'Descrição da Tarefa 2', '2022-02-01', '2022-02-15', 1, 2),
    (3, 'Tarefa 3', 'Descrição da Tarefa 3', '2022-03-01', '2022-03-31', 2, 3),
    (4, 'Tarefa 4', 'Descrição da Tarefa 4', '2022-04-01', '2022-04-30', 2, 4),
    (5, 'Tarefa 5', 'Descrição da Tarefa 5', '2022-05-01', '2022-05-31', 3, 5);

-- Consulta complexa
SELECT departamento.nome AS nome_departamento, funcionario.nome AS nome_funcionario, projeto.nome AS nome_projeto, tarefa.nome AS nome_tarefa
FROM departamento
INNER JOIN funcionario ON departamento.id = funcionario.departamento_id
INNER JOIN projeto ON departamento.id = projeto.departamento_id
INNER JOIN tarefa ON projeto.id = tarefa.projeto_id
WHERE funcionario.salario > 5000.00
ORDER BY departamento.nome, funcionario.nome, projeto.nome, tarefa.nome;
```

Neste código SQL, criamos quatro tabelas: departamento, funcionario, projeto e tarefa. Em seguida, inserimos dados nessas tabelas para simular um ambiente de negócios.

A consulta complexa utiliza as cláusulas INNER JOIN para juntar as tabelas e a cláusula WHERE para filtrar os funcionários com salários acima de 5000.00. A consulta também faz uso da cláusula ORDER BY para ordenar os resultados por nome do departamento, nome do funcionário, nome do projeto e nome da tarefa.

Espero que isso atenda às suas expectativas! Se você tiver mais alguma solicitação, fique à vontade para me perguntar.