Claro, vou gerar um código complexo em COOL (Cool é uma linguagem de programação orientada a objetos desenvolvida na Universidade de Berkeley) para você:

```
class Fatorial {

	// Função recursiva para cálculo do fatorial
	method factorial(n: Int) : Int {
		if (n <= 1) then
			return 1;
		else
			return n * factorial(n-1);
		fi;
	};

	// Função para exibição dos resultados
	method printResults(n: Int, result: Int) : Object {
		out_string("O fatorial de ");
		out_int(n);
		out_string(" é ");
		out_int(result);
		out_string("\n");
		return self;
	};

	// Função principal
	main() : Object {
		let n: Int <- 5; // Valor do fatorial a ser calculado
		let result: Int <- factorial(n); // Chamada da função factorial
		self.printResults(n, result); // Chamada da função printResults
		return self;
	};
};

let fatorial: Fatorial <- Fatorial.new(); // Instância da classe Fatorial
fatorial.main(); // Chamada da função principal
```

Este código em COOL implementa o cálculo do fatorial de um número utilizando recursão. A classe `Fatorial` contém dois métodos: `factorial` e `printResults`. 

O método `factorial` é uma função recursiva que calcula o fatorial de um número `n`. Ele verifica se `n` é menor ou igual a 1 e retorna 1 nesse caso. Caso contrário, ele retorna `n` multiplicado pelo fatorial de `n-1`.

O método `printResults` é responsável por imprimir os resultados do cálculo. Ele recebe como parâmetro o valor `n` (o número para o qual o fatorial foi calculado) e o resultado do cálculo. Ele imprime uma mensagem formatada indicando o valor de `n` e o resultado do fatorial.

A função principal `main` instancia a classe `Fatorial`, define o valor de `n` como 5, chama o método `factorial` para calcular o fatorial de `n` e em seguida chama o método `printResults` para exibir os resultados.

Por fim, é criada uma instância da classe `Fatorial` chamada `fatorial` e é chamado o método `main` para executar o programa.