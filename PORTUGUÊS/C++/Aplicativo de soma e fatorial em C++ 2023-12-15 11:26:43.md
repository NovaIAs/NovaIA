void imprimirMensagem(string mensagem) {
    cout << mensagem << endl;
}

int somarNumeros(int num1, int num2) {
    return num1 + num2;
}

int calcularFatorial(int n) {
    if(n == 0 || n == 1)
        return 1;
    else
        return n * calcularFatorial(n - 1);
}

int main() {
    imprimirMensagem("Bem-vindo ao programa!");

    int numero1, numero2;
    cout << "Digite o primeiro número: ";
    cin >> numero1;
    cout << "Digite o segundo número: ";
    cin >> numero2;

    imprimirMensagem("A soma dos números é: " + to_string(somarNumeros(numero1, numero2)));

    int numeroFatorial;
    cout << "Digite um número para calcular o fatorial: ";
    cin >> numeroFatorial;

    imprimirMensagem("O fatorial de " + to_string(numeroFatorial) + " é: " + to_string(calcularFatorial(numeroFatorial)));

    return 0;
}
```

Neste código em C++, temos uma função `imprimirMensagem` que recebe uma mensagem como parâmetro e a imprime na tela. Em seguida, temos uma função `somarNumeros` que recebe dois números como parâmetros e retorna a soma deles. Também temos uma função `calcularFatorial` que recebe um número como parâmetro e retorna o fatorial desse número.

No `main`, primeiro chamamos a função `imprimirMensagem` para exibir uma mensagem de boas-vindas ao programa. Em seguida, pedimos ao usuário para digitar dois números e os armazenamos nas variáveis `numero1` e `numero2`. Chamamos então a função `somarNumeros` passando esses números como argumentos e exibimos o resultado.

Depois, pedimos ao usuário para digitar um número para calcular o fatorial e armazenamos esse número na variável `numeroFatorial`. Chamamos então a função `calcularFatorial` passando esse número como argumento e exibimos o resultado.

Por fim, retornamos 0 para indicar que o programa foi executado com sucesso.