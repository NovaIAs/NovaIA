Claro, vou gerar um código complexo em PHP para você. Este código irá criar um sistema de gerenciamento de tarefas, onde os usuários poderão adicionar, editar e excluir suas tarefas. Vou dividi-lo em partes e explicar cada uma delas:

Parte 1: Configuração do banco de dados

```php
<?php
$servername = "localhost";
$username = "seu_usuario";
$password = "sua_senha";
$dbname = "nome_do_banco";

$conn = new mysqli($servername, $username, $password, $dbname);

if ($conn->connect_error) {
    die("Falha na conexão com o banco de dados: " . $conn->connect_error);
}
?>
```

Nesta parte, estamos configurando a conexão com o banco de dados MySQL. Você deve substituir "seu_usuario", "sua_senha" e "nome_do_banco" pelos seus dados de acesso ao banco de dados.

Parte 2: Criação da tabela de tarefas

```php
<?php
$sql = "CREATE TABLE IF NOT EXISTS tarefas (
    id INT(6) UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    titulo VARCHAR(50) NOT NULL,
    descricao TEXT,
    data_criacao TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    data_atualizacao TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)";

if ($conn->query($sql) === FALSE) {
    echo "Erro ao criar a tabela: " . $conn->error;
}
?>
```

Nesta parte, estamos criando a tabela "tarefas" no banco de dados. A tabela possui colunas para o ID da tarefa (chave primária), título, descrição, data de criação e data de atualização.

Parte 3: Página de listagem das tarefas

```php
<?php
$sql = "SELECT * FROM tarefas";
$result = $conn->query($sql);

if ($result->num_rows > 0) {
    while ($row = $result->fetch_assoc()) {
        echo "ID: " . $row["id"]. " - Título: " . $row["titulo"]. "<br>";
    }
} else {
    echo "Nenhuma tarefa encontrada.";
}
?>
```

Nesta parte, estamos consultando todas as tarefas cadastradas e exibindo seus títulos na página. Caso não haja nenhuma tarefa cadastrada, será exibida a mensagem "Nenhuma tarefa encontrada."

Parte 4: Página de adição de tarefas

```php
<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $titulo = $_POST["titulo"];
    $descricao = $_POST["descricao"];

    $sql = "INSERT INTO tarefas (titulo, descricao) VALUES ('$titulo', '$descricao')";

    if ($conn->query($sql) === TRUE) {
        echo "Tarefa adicionada com sucesso!";
    } else {
        echo "Erro ao adicionar a tarefa: " . $conn->error;
    }
}
?>

<form method="POST" action="">
    <label for="titulo">Título:</label><br>
    <input type="text" id="titulo" name="titulo" required><br><br>

    <label for="descricao">Descrição:</label><br>
    <textarea id="descricao" name="descricao"></textarea><br><br>

    <input type="submit" value="Adicionar tarefa">
</form>
```

Nesta parte, estamos criando um formulário para adicionar novas tarefas. Quando o formulário for enviado (método POST), os dados serão salvos no banco de dados. Caso ocorra algum erro, será exibida uma mensagem de erro.

Parte 5: Página de edição de tarefas

```php
<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $id = $_POST["id"];
    $titulo = $_POST["titulo"];
    $descricao = $_POST["descricao"];

    $sql = "UPDATE tarefas SET titulo='$titulo', descricao='$descricao' WHERE id='$id'";

    if ($conn->query($sql) === TRUE) {
        echo "Tarefa atualizada com sucesso!";
    } else {
        echo "Erro ao atualizar a tarefa: " . $conn->error;
    }
}

if (isset($_GET["id"])) {
    $id = $_GET["id"];

    $sql = "SELECT * FROM tarefas WHERE id='$id'";
    $result = $conn->query($sql);

    if ($result->num_rows > 0) {
        $row = $result->fetch_assoc();
        $titulo = $row["titulo"];
        $descricao = $row["descricao"];
    } else {
        echo "Nenhuma tarefa encontrada.";
    }
}
?>

<form method="POST" action="">
    <input type="hidden" name="id" value="<?php echo $id; ?>">

    <label for="titulo">Título:</label><br>
    <input type="text" id="titulo" name="titulo" value="<?php echo $titulo; ?>" required><br><br>

    <label for="descricao">Descrição:</label><br>
    <textarea id="descricao" name="descricao"><?php echo $descricao; ?></textarea><br><br>

    <input type="submit" value="Atualizar tarefa">
</form>
```

Nesta parte, estamos criando um formulário para editar tarefas existentes. Quando o formulário for enviado (método POST), os dados serão atualizados no banco de dados. Caso ocorra algum erro, será exibida uma mensagem de erro. Antes de exibir o formulário de edição, verificamos se foi passado o parâmetro "id" pela URL para buscar os dados da tarefa correspondente.

Parte 6: Página de exclusão de tarefas

```php
<?php
if (isset($_GET["id"])) {
    $id = $_GET["id"];

    $sql = "DELETE FROM tarefas WHERE id='$id'";

    if ($conn->query($sql) === TRUE) {
        echo "Tarefa excluída com sucesso!";
    } else {
        echo "Erro ao excluir a tarefa: " . $conn->error;
    }
}
?>

<ul>
<?php
$sql = "SELECT * FROM tarefas";
$result = $conn->query($sql);

if ($result->num_rows > 0) {
    while ($row = $result->fetch_assoc()) {
        echo "<li>ID: " . $row["id"]. " - Título: " . $row["titulo"]. " <a href='?id=" . $row["id"] . "'>Excluir</a></li>";
    }
} else {
    echo "Nenhuma tarefa encontrada.";
}
?>
</ul>
```

Nesta parte, estamos exibindo a lista de tarefas com um link de exclusão ao lado. Quando o link for clicado, a tarefa correspondente será excluída do banco de dados. Caso ocorra algum erro, será exibida uma mensagem de erro.

Este é um exemplo de um código complexo em PHP para um sistema de gerenciamento de tarefas. Ele abrange a criação e configuração do banco de dados, a criação das tabelas, a listagem, adição, edição e exclusão de tarefas. Lembre-se de adaptar as configurações do banco de dados de acordo com suas necessidades.