Claro! Aqui está um exemplo de um código complexo em D:

```d
import std.algorithm;
import std.stdio;

struct Pessoa {
    string nome;
    int idade;
    float altura;
}

void main() {
    Pessoa[] pessoas;

    // Preenche o vetor de pessoas com dados fictícios
    pessoas ~= Pessoa("João", 25, 1.75);
    pessoas ~= Pessoa("Maria", 30, 1.60);
    pessoas ~= Pessoa("Lucas", 20, 1.80);
    pessoas ~= Pessoa("Ana", 35, 1.70);

    // Ordena as pessoas pelo nome em ordem crescente
    pessoas.sort!((a, b) => a.nome < b.nome);

    // Imprime as pessoas ordenadas
    writeln("Pessoas ordenadas pelo nome:");
    foreach (pessoa; pessoas) {
        writeln(pessoa.nome, ", ", pessoa.idade, " anos, ", pessoa.altura, "m");
    }

    // Filtra as pessoas com idade maior que 25
    auto pessoasMaiores = pessoas.filter!(p => p.idade > 25);

    // Imprime as pessoas filtradas
    writeln("\nPessoas com idade maior que 25:");
    foreach (pessoa; pessoasMaiores) {
        writeln(pessoa.nome, ", ", pessoa.idade, " anos, ", pessoa.altura, "m");
    }

    // Calcula a média de altura das pessoas
    auto mediaAltura = pessoas.map!(p => p.altura).sum / pessoas.length;

    // Imprime a média de altura
    writeln("\nMédia de altura das pessoas: ", mediaAltura, "m");
}
```

Neste código em D, temos uma estrutura `Pessoa` que representa informações sobre uma pessoa, como nome, idade e altura. Em seguida, temos a função `main`, onde criamos um vetor de pessoas e preenchemos com dados fictícios.

Em seguida, utilizamos o algoritmo `sort` da biblioteca padrão para ordenar as pessoas pelo nome em ordem crescente. Depois, imprimimos as pessoas ordenadas na saída.

Continuando, utilizamos o algoritmo `filter` para filtrar as pessoas com idade maior que 25, armazenando o resultado em um novo vetor chamado `pessoasMaiores`. Em seguida, imprimimos as pessoas filtradas na saída.

Após isso, utilizamos o algoritmo `map` para mapear a altura de cada pessoa e obter um novo vetor contendo apenas as alturas. Em seguida, calculamos a média das alturas utilizando a função `sum` e dividindo pelo tamanho do vetor original. Por fim, imprimimos a média de altura na saída.

Espero que este exemplo atenda às suas expectativas! Se tiver mais alguma dúvida ou precisar de outro código, é só me pedir.