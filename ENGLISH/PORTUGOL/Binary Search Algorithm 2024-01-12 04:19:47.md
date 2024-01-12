```portugol
program algoritmo_busca_binaria;
const
  tam_max = 100;
type
  vetor = array[1..tam_max] of integer;
var
  vet: vetor;
  n, i, pos: integer;

procedure imprimir_vetor;
var
  j: integer;
begin
  writeln('Vetor:');
  for j := 1 to n do
    write(vet[j], ' ');
  writeln;
end procedure;

procedure preencher_vetor;
var
  j: integer;
begin
  for j := 1 to n do
    vet[j] := random(100);
end procedure;

procedure ordenar_vetor;
var
  j, k, aux: integer;
begin
  for j := 1 to n - 1 do
    for k := j + 1 to n do
      if vet[j] > vet[k] then
        begin
          aux := vet[j];
          vet[j] := vet[k];
          vet[k] := aux;
        end;
end procedure;

procedure buscar_binaria(x: integer; var pos: integer);
var
  primeiro, ultimo, meio: integer;
begin
  primeiro := 1;
  ultimo := n;
  while primeiro <= ultimo do
    begin
      meio := (primeiro + ultimo) div 2;
      if x = vet[meio] then
        begin
          pos := meio;
          return;
        end
      else
        if x < vet[meio] then
          ultimo := meio - 1
        else
          primeiro := meio + 1;
    end;
  pos := 0;
end procedure;

begin
  writeln('Digite o tamanho do vetor:');
  readln(n);

  preencher_vetor;
  imprimir_vetor;

  ordenar_vetor;
  imprimir_vetor;

  writeln('Digite o número que deseja buscar:');
  readln(x);

  buscar_binaria(x, pos);

  if pos = 0 then
    writeln('Número não encontrado.')
  else
    writeln('Número encontrado na posição ', pos, '.');
end.
```

Explanation:

* The program begins by defining a constant `tam_max` which sets the maximum size of the vector.
* It then defines a type `vetor` which is an array of integers with a maximum size of `tam_max`.
* The program then declares three variables: `vet` which is an array of integers, `n` which is the size of the vector, and `i` and `pos` which are used for iteration and to store the position of the searched element.
* The program then defines three procedures: `imprimir_vetor` which prints the vector, `preencher_vetor` which fills the vector with random numbers, and `ordenar_vetor` which sorts the vector in ascending order.
* The program then defines a procedure `buscar_binaria` which performs a binary search on the vector for a given element.
* The main program begins by prompting the user to enter the size of the vector.
* It then calls the `preencher_vetor` procedure to fill the vector with random numbers and the `imprimir_vetor` procedure to print the vector.
* It then calls the `ordenar_vetor` procedure to sort the vector and the `imprimir_vetor` procedure to print the sorted vector.
* It then prompts the user to enter the number to be searched for.
* It then calls the `buscar_binaria` procedure to search for the number in the vector and stores the position of the number in the variable `pos`.
* If the number is not found, the program prints a message indicating that the number was not found.
* If the number is found, the program prints a message indicating the position of the number in the vector.