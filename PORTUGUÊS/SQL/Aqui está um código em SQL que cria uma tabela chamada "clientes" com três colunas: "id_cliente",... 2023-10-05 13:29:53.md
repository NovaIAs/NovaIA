Claro! Aqui está um exemplo de um código complexo em SQL que envolve múltiplas tabelas e operações:

```
-- Criação das Tabelas
CREATE TABLE departamento (
    id_departamento INT,
    nome_departamento VARCHAR(50),
    PRIMARY KEY (id_departamento)
);

CREATE TABLE funcionario (
    id_funcionario INT,
    nome_funcionario VARCHAR(50),
    id_departamento INT,
    PRIMARY KEY (id_funcionario),
    FOREIGN KEY (id_departamento) REFERENCES departamento(id_departamento)
);

CREATE TABLE projeto (
    id_projeto INT,
    nome_projeto VARCHAR(50),
    PRIMARY KEY (id_projeto)
);

CREATE TABLE responsavel (
    id_funcionario INT,
    id_projeto INT,
    PRIMARY KEY (id_funcionario, id_projeto),
    FOREIGN KEY (id_funcionario) REFERENCES funcionario(id_funcionario),
    FOREIGN KEY (id_projeto) REFERENCES projeto(id_projeto)
);

-- Inserção de Dados
INSERT INTO departamento (id_departamento, nome_departamento)
VALUES (1, 'Vendas'), (2, 'Marketing'), (3, 'TI');

INSERT INTO funcionario (id_funcionario, nome_funcionario, id_departamento)
VALUES (1, 'João', 1), (2, 'Maria', 2), (3, 'Pedro', 3);

INSERT INTO projeto (id_projeto, nome_projeto)
VALUES (1, 'Projeto A'), (2, 'Projeto B'), (3, 'Projeto C');

INSERT INTO responsavel (id_funcionario, id_projeto)
VALUES (1, 1), (2, 2), (3, 3);

-- Consulta Complexa
SELECT f.nome_funcionario, d.nome_departamento, p.nome_projeto
FROM funcionario f
JOIN departamento d ON f.id_departamento = d.id_departamento
JOIN responsavel r ON f.id_funcionario = r.id_funcionario
JOIN projeto p ON r.id_projeto = p.id_projeto
WHERE d.nome_departamento = 'Vendas'
ORDER BY p.nome_projeto, f.nome_funcionario;
```

Neste código em SQL, estamos criando e populando quatro tabelas: "departamento", "funcionario", "projeto" e "responsavel". A tabela "departamento" armazena informações sobre os departamentos de uma empresa, a tabela "funcionario" contém dados dos funcionários, a tabela "projeto" guarda informações sobre os projetos em andamento e a tabela "responsavel" relaciona os funcionários responsáveis por cada projeto.

Em seguida, inserimos alguns registros nessas tabelas para exemplificar os dados.

Por fim, realizamos uma consulta complexa que retorna o nome dos funcionários, nome dos departamentos e nome dos projetos para funcionários que pertencem ao departamento de Vendas. Os resultados são ordenados pelo nome do projeto e, em casos de projetos com o mesmo nome, ordenados pelo nome do funcionário.

Espero que esse código SQL complexo atenda às suas expectativas!