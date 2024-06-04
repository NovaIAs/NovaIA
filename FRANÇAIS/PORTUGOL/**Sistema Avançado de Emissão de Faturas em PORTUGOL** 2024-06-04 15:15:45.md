**Programme de facturation avancé en PORTUGOL**

```portugol
// Define os tipos de dados dos itens da fatura
Tipo ItemFatura = Registro
    Descricao: Cadeia;
    Quantidade: Real;
    ValorUnitario: Real;
FimRegistro;

// Define o tipo de dados da fatura
Tipo Fatura = Registro
    Numero: Inteiro;
    DataEmissao: Data;
    Cliente: Registro
        Nome: Cadeia;
        Endereco: Cadeia;
        Cidade: Cadeia;
        CEP: Cadeia;
    FimRegistro;
    Itens: Vetor [1..100] de ItemFatura;
    Total: Real;
FimRegistro;

// Função para criar uma nova fatura
Funcao CriarFatura(): Fatura
    Var
        fatura: Fatura;
    Inicio
        fatura.Numero := 0;
        fatura.DataEmissao := Data();
        fatura.Cliente.Nome := Cadeia();
        fatura.Cliente.Endereco := Cadeia();
        fatura.Cliente.Cidade := Cadeia();
        fatura.Cliente.CEP := Cadeia();
        fatura.Itens := [];
        fatura.Total := 0;
        Retorne fatura;
    FimFuncao;

// Função para adicionar um item à fatura
Procedimento AdicionarItem(fatura: Fatura; item: ItemFatura)
    Var
        indice: Inteiro;
    Inicio
        indice := Tamanho(fatura.Itens) + 1;
        fatura.Itens[indice] := item;
        fatura.Total := fatura.Total + (item.Quantidade * item.ValorUnitario);
    FimProcedimento;

// Função para imprimir a fatura
Procedimento ImprimirFatura(fatura: Fatura)
    Var
        i: Inteiro;
        linha: Cadeia;
    Inicio
        Escreva("Fatura Número: ", fatura.Numero);
        Escreva("Data de Emissão: ", FormatarData(fatura.DataEmissao));
        Escreva("Cliente: ", fatura.Cliente.Nome);
        Escreva("Endereço: ", fatura.Cliente.Endereco);
        Escreva("Cidade: ", fatura.Cliente.Cidade);
        Escreva("CEP: ", fatura.Cliente.CEP);
        Escreva("--------------------------------------------------");
        Escreva("Descrição", 30, "Quantidade", 10, "Valor Unitário", 15);
        Escreva("--------------------------------------------------");
        Para i de 1 até Tamanho(fatura.Itens)
            linha := FormatarCadeia(fatura.Itens[i].Descricao, 30);
            linha := linha + FormatarReal(fatura.Itens[i].Quantidade, 10);
            linha := linha + FormatarReal(fatura.Itens[i].ValorUnitario, 15);
            Escreva(linha);
        FimPara;
        Escreva("--------------------------------------------------");
        Escreva("Total: ", FormatarReal(fatura.Total));
    FimProcedimento;

// Código principal
Inicio
    Var
        fatura: Fatura;
    Inicio
        // Cria uma nova fatura
        fatura := CriarFatura();

        // Define as informações do cliente
        fatura.Cliente.Nome := "João da Silva";
        fatura.Cliente.Endereco := "Rua das Flores, 123";
        fatura.Cliente.Cidade := "São Paulo";
        fatura.Cliente.CEP := "01234-567";

        // Adiciona itens à fatura
        AdicionarItem(fatura, ItemFatura("Produto A", 10, 10));
        AdicionarItem(fatura, ItemFatura("Produto B", 5, 15));
        AdicionarItem(fatura, ItemFatura("Produto C", 2, 20));

        // Imprime a fatura
        ImprimirFatura(fatura);
    FimInicio
Fim.
```

**Explicação do código:**

* **Tipos de dados personalizados:** São definidos tipos de dados personalizados para representar itens de fatura (`ItemFatura`) e faturas (`Fatura`).
* **Funções e procedimentos:** O programa usa funções para criar novas faturas (`CriarFatura`) e adicionar itens a elas (`AdicionarItem`). Também usa um procedimento para imprimir faturas (`ImprimirFatura`).
* **Armazenamento de dados:** As informações da fatura, incluindo itens e informações do cliente, são armazenadas em estruturas de dados vetoriais e registros.
* **Formatação de saída:** O código usa funções para formatar dados como valores moneários e datas, facilitando a leitura da fatura.
* **Manipulação de strings:** O código também usa funções para manipular strings, como concatenar e formatar dados para impressão.
* **Código modular:** O programa é modularizado em funções e procedimentos, o que melhora a legibilidade e a manutenção do código.