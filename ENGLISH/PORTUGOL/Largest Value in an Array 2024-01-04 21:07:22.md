```portugol
program maior_numero_array;

{Declaração de variáveis.}
integer array valores[10];
integer maior, i;

{Inicialização das variáveis.}
maior := 0;
i := 0;

{Leitura dos valores do array.}
for i := 0 to 9 do
{Está fazendo a leitura do array com o comando READ, e armazenar os valores lidos no array VALORES através do comando ARRAY}
    read(valores[i]);

{Busca pelo maior valor no array.}
for i := 0 to 9 do
{está verificando se os valores no array VALORES são maiores que o valor na variável MAIOR. Se for maior, o valor é armazenado na variável MAIOR.}
    if valores[i] > maior then
        maior := valores[i];

{Exibição do maior valor encontrado.}
write("O maior valor é: ");
write(maior);
```

Explanation:

1. **Variable Declarations:**
   - `integer array valores[10];`: Declares an integer array named `valores` with a size of 10, which can store 10 integer values.
   - `integer maior, i;`: Declares two integer variables: `maior` to store the largest value found in the array and `i` as a loop counter.

2. **Variable Initialization:**
   - `maior := 0;`: Initializes the `maior` variable to 0. This is done to ensure that any valid value in the array will be greater than 0.
   - `i := 0;`: Initializes the `i` variable to 0, which will be used as the index for iterating through the array.

3. **Reading Array Values:**
   - `for i := 0 to 9 do read(valores[i]);`: This loop reads values from the user and stores them in the `valores` array. The loop runs from index 0 to 9, covering all 10 elements of the array. The `read` command is used to read a value from the user.

4. **Finding the Largest Value:**
   - `for i := 0 to 9 do if valores[i] > maior then maior := valores[i];`: This loop iterates through the `valores` array and compares each element with the current largest value stored in the `maior` variable. If an element is greater than `maior`, it updates the `maior` variable to the new larger value.

5. **Displaying the Largest Value:**
   - `write("O maior valor é: "); write(maior);`: Finally, the code displays the largest value found in the array using the `write` command. It prints "O maior valor é: " followed by the value stored in the `maior` variable.