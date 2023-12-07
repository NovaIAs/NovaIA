Claro! Aqui está um código complexo em Modula-2, utilizando a estrutura de uma agenda telefônica com capacidade para armazenar até 100 contatos e implementando algumas funcionalidades adicionais. Vou explicar cada parte do código para que você possa compreender sua funcionalidade.

```modula-2
MODULE AgendaTelefonica;

FROM InOut IMPORT ReadString, WriteString;
FROM Machine IMPORT Init;

TYPE
    Nome = ARRAY [1..50] OF CHAR;
    Telefone = ARRAY [1..15] OF CHAR;
    Contato = RECORD
        nome: Nome;
        telefone: Telefone;
    END;

CONST
    MAX_CONTATOS = 100;

VAR
    agenda: ARRAY [1..MAX_CONTATOS] OF Contato;
    totalContatos: INTEGER;

PROCEDURE InserirContato(novoNome: Nome; novoTelefone: Telefone);
BEGIN
    IF totalContatos < MAX_CONTATOS THEN
        INC(totalContatos);
        agenda[totalContatos].nome := novoNome;
        agenda[totalContatos].telefone := novoTelefone;
        WriteString("Contato inserido com sucesso!");
    ELSE
        WriteString("A agenda está cheia, não é possível adicionar mais contatos.");
    END;
END InserirContato;

PROCEDURE PesquisarContato(pesquisa: Nome);
VAR
    encontrado: BOOLEAN;
    i: INTEGER;
BEGIN
    encontrado := FALSE;
    FOR i := 1 TO totalContatos DO
        IF pesquisa = agenda[i].nome THEN
            encontrado := TRUE;
            WriteString("Nome: ");
            WriteString(agenda[i].nome);
            WriteString("Telefone: ");
            WriteString(agenda[i].telefone);
        END;
    END;

    IF NOT encontrado THEN
        WriteString("Contato não encontrado.");
    END;
END PesquisarContato;

PROCEDURE RemoverContato(removerNome: Nome);
VAR
    encontrado: BOOLEAN;
    i: INTEGER;
BEGIN
    encontrado := FALSE;
    FOR i := 1 TO totalContatos DO
        IF removerNome = agenda[i].nome THEN
            encontrado := TRUE;
            agenda[i].nome := agenda[totalContatos].nome;
            agenda[i].telefone := agenda[totalContatos].telefone;
            DEC(totalContatos);
        END;
    END;

    IF encontrado THEN
        WriteString("Contato removido com sucesso!");
    ELSE
        WriteString("Contato não encontrado.");
    END;
END RemoverContato;

PROCEDURE ListarContatos;
VAR
    i: INTEGER;
BEGIN
    IF totalContatos = 0 THEN
        WriteString("A agenda está vazia.");
    ELSE
        FOR i := 1 TO totalContatos DO
            WriteString("Nome: ");
            WriteString(agenda[i].nome);
            WriteString("Telefone: ");
            WriteString(agenda[i].telefone);
        END;
    END;
END ListarContatos;

PROCEDURE Main;
VAR
    opcao: CHAR;
    nome: Nome;
    telefone: Telefone;
BEGIN
    totalContatos := 0;
    Init();

    REPEAT
        WriteString("Escolha uma opção:");
        WriteString("1 - Inserir novo contato");
        WriteString("2 - Pesquisar contato");
        WriteString("3 - Remover contato");
        WriteString("4 - Listar contatos");
        WriteString("0 - Sair");
        opcao := ReadString();

        CASE opcao OF
            "1": BEGIN
                    WriteString("Digite o nome do contato:");
                    nome := ReadString();
                    WriteString("Digite o telefone do contato:");
                    telefone := ReadString();
                    InserirContato(nome, telefone);
                END;
            "2": BEGIN
                    WriteString("Digite o nome do contato a ser pesquisado:");
                    nome := ReadString();
                    PesquisarContato(nome);
                END;
            "3": BEGIN
                    WriteString("Digite o nome do contato a ser removido:");
                    nome := ReadString();
                    RemoverContato(nome);
                END;
            "4": ListarContatos;
        END;
    UNTIL opcao = "0";
END Main;

BEGIN
    Main;
END AgendaTelefonica.
```

Aqui, criamos uma agenda telefônica usando um registro chamado `Contato` com campos para o nome e telefone. Definimos uma constante `MAX_CONTATOS` para limitar o número de contatos que a agenda pode armazenar (100 no exemplo).

Em seguida, temos o procedimento `InserirContato`, que insere um novo contato na agenda, desde que ainda haja espaço disponível. Caso a agenda esteja cheia, uma mensagem de erro será exibida.

O procedimento `PesquisarContato` recebe um nome como parâmetro e busca na agenda por contatos com o mesmo nome. Se encontrado, os detalhes do contato são exibidos, caso contrário, uma mensagem de erro é mostrada.

O procedimento `RemoverContato` recebe um nome como parâmetro e remove o contato correspondente da agenda, ajustando a lista de contatos. Novamente, uma mensagem de erro é exibida se o contato não for encontrado.

O procedimento `ListarContatos` exibe todos os contatos armazenados na agenda, um por vez.

Por fim, temos o procedimento `Main`, que implementa um menu de opções para interagir com a agenda telefônica. As opções são exibidas, o usuário escolhe uma delas e as ações são realizadas conforme a escolha.

Espero que este código em Modula-2 atenda às suas expectativas!