Claro! Aqui está um exemplo de um código complexo em TypeScript que envolve a criação de uma classe de matriz bidimensional:

```typescript
class Matriz {
    private linhas: number;
    private colunas: number;
    private matriz: number[][];

    constructor(linhas: number, colunas: number) {
        this.linhas = linhas;
        this.colunas = colunas;
        this.matriz = new Array(linhas);

        for (let i = 0; i < linhas; i++) {
            this.matriz[i] = new Array(colunas);
        }
    }

    preencherMatriz(valor: number) {
        for (let i = 0; i < this.linhas; i++) {
            for (let j = 0; j < this.colunas; j++) {
                this.matriz[i][j] = valor;
            }
        }
    }

    exibirMatriz() {
        for (let i = 0; i < this.linhas; i++) {
            let linha = "";
            for (let j = 0; j < this.colunas; j++) {
                linha += this.matriz[i][j] + " ";
            }
            console.log(linha);
        }
    }
}

const minhaMatriz = new Matriz(3, 4);
minhaMatriz.preencherMatriz(5);
minhaMatriz.exibirMatriz();
```

Neste código, temos a classe `Matriz` que representa uma matriz bidimensional. A classe possui os atributos `linhas` e `colunas` para armazenar as dimensões da matriz e a matriz em si, representada pelo array `matriz`. 

No construtor da classe, criamos o array `matriz` com o número de linhas e colunas especificados, e inicializamos cada elemento com `undefined`. 

O método `preencherMatriz` recebe um valor como parâmetro e itera sobre a matriz, atribuindo o valor fornecido a cada elemento. 

O método `exibirMatriz` percorre a matriz e imprime cada elemento no console, formatando-os em linhas separadas.

Por fim, criamos uma instância da classe `Matriz`, definimos suas dimensões como 3 linhas e 4 colunas, e preenchemos toda a matriz com o valor 5. Em seguida, chamamos o método `exibirMatriz` para visualizar o resultado.

Espero que este exemplo atenda às suas expectativas. Lembre-se de que você pode personalizar o código conforme suas necessidades!