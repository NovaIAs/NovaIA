```portugol
program ManipulaMatriz

type

    Inteiro = integer;

    Real = double;

    Caracter = char;

    Boleano = boolean;

    Matriz = array[Inteiro, Inteiro] of Real;

procedure ImprimeMatriz(Matriz: Matriz; NLinhas, NColunas: Inteiro)

var

    i, j: Inteiro;

begin

    for i := 0 to NLinhas - 1 do begin

        for j := 0 to NColunas - 1 do begin

            write(Matriz[i, j]:6:2);

        end;

        writeln;

    end;

end;

procedure TranspostaMatriz(Matriz: Matriz; NLinhas, NColunas: Inteiro; var Transposta: Matriz)

var

    i, j: Inteiro;

begin

    for i := 0 to NLinhas - 1 do begin

        for j := 0 to NColunas - 1 do begin

            Transposta[j, i] := Matriz[i, j];

        end;

    end;

end;

procedure SomaMatrizes(A, B: Matriz; NLinhas, NColunas: Inteiro; var Soma: Matriz)

var

    i, j: Inteiro;

begin

    for i := 0 to NLinhas - 1 do begin

        for j := 0 to NColunas - 1 do begin

            Soma[i, j] := A[i, j] + B[i, j];

        end;

    end;

end;

procedure MultiplicaMatrizPorEscalar(Matriz: Matriz; NLinhas, NColunas: Inteiro; Escalar: Real; var Produto: Matriz)

var

    i, j: Inteiro;

begin

    for i := 0 to NLinhas - 1 do begin

        for j := 0 to NColunas - 1 do begin

            Produto[i, j] := Matriz[i, j] * Escalar;

        end;

    end;

end;

procedure MultiplicaMatrizes(A, B: Matriz; NLinhasA, NColunasA, NLinhasB, NColunasB: Inteiro; var Produto: Matriz)

var

    i, j, k: Inteiro;

begin

    if NColunasA <> NLinhasB then begin

        writef('As matrizes não podem ser multiplicadas');

    end else begin

        for i := 0 to NLinhasA - 1 do begin

            for j := 0 to NColunasB - 1 do begin

                Produto[i, j] := 0;

                for k := 0 to NColunasA - 1 do begin

                    Produto[i, j] := Produto[i, j] + A[i, k] * B[k, j];

                end;

            end;

        end;

    end;

end;

procedure InversaMatriz(Matriz: Matriz; N: Inteiro; var Inversa: Matriz)

var

    i, j, k: Inteiro;

    Determinante: Real;

    MatrizCofatores: Matriz;

    MatrizAdjugada: Matriz;

begin

    Determinante := 0;

    for j := 0 to N - 1 do begin

        Determinante := Determinante + Matriz[0, j] * Cofator(Matriz, 0, j);

    end;

    if Determinante = 0 then begin

        writef('A matriz não é invertível');

    end else begin

        for i := 0 to N - 1 do begin

            for j := 0 to N - 1 do begin

                MatrizCofatores[i, j] := Cofator(Matriz, i, j);

            end;

        end;

        TranspostaMatriz(MatrizCofatores, N, MatrizAdjugada);

        MultiplicaMatrizPorEscalar(MatrizAdjugada, N, 1 / Determinante, Inversa);

    end;

end;

function Cofator(Matriz: Matriz; Linha, Coluna: Inteiro): Real

var

    i, j: Inteiro;

begin

    Cofator := 0;

    if (Linha + Coluna) mod 2 = 0 then begin

        Cofator := Cofator + Matriz[Linha + 1, Coluna + 1] * Menor(Matriz, Linha + 1, Coluna + 1);

        Cofator := Cofator - Matriz[Linha + 1, Coluna] * Menor(Matriz, Linha + 1, Coluna);

        Cofator := Cofator + Matriz[Linha, Coluna + 1] * Menor(Matriz, Linha, Coluna + 1);

        Cofator := Cofator - Matriz[Linha, Coluna] * Menor(Matriz, Linha, Coluna);

    end else begin

        Cofator := Cofator - Matriz[Linha + 1, Coluna + 1] * Menor(Matriz, Linha + 1, Coluna + 1);

        Cofator := Cofator + Matriz[Linha + 1, Coluna] * Menor(Matriz, Linha + 1, Coluna);

        Cofator := Cofator - Matriz[Linha, Coluna + 1] * Menor(Matriz, Linha, Coluna + 1);

        Cofator := Cofator + Matriz[Linha, Coluna] * Menor(Matriz, Linha, Coluna);

    end;

end;

function Menor(Matriz: Matriz; Linha, Coluna: Inteiro): Real

var

    i, j: Inteiro;

    Menor: Matriz;

begin

    for i := 0 to Linha - 1 do begin

        for j := 0 to Coluna - 1 do begin

            Menor[i, j] := Matriz[i, j];

        end;

    end;

    for i := Linha + 1 to 2 do begin

        for j := 0 to Coluna - 1 do begin

            Menor[i - 1, j] := Matriz[i, j];

        end;

    end;

    for i := 0 to Linha - 1 do begin

        for j := Coluna + 1 to 2 do begin

            Menor[i, j - 1] := Matriz[i, j];

        end;

    end;

    for i := Linha + 1 to 2 do begin

        for j := Coluna + 1 to 2 do begin

            Menor[i - 1, j - 1] := Matriz[i, j];

        end;

    end;

    Menor := Determinante(Menor, 2);

end;

function Determinante(Matriz: Matriz; N: Inteiro): Real

var

    i, j, k: Inteiro;

    Determinante: Real;

    MatrizCofatores: Matriz;

begin

    Determinante := 0;

    for j := 0 to N - 1 do begin

        Determinante := Determinante + Matriz[0, j] * Cofator(Matriz, 0, j);

    end;

end;

begin

    var

        A, B, C, D, E, F, G, H, I, J, K, L: Matriz;

        TranspostaA, TranspostaB, TranspostaC, TranspostaD, TranspostaE, TranspostaF, TranspostaG, TranspostaH, TranspostaI, TranspostaJ, TranspostaK, TranspostaL: Matriz;

        SomaAB, SomaCD, SomaEF, SomaGH, SomaIJ, SomaKL: Matriz;

        ProdutoAB, ProdutoCD, ProdutoEF, ProdutoGH, ProdutoIJ, ProdutoKL: Matriz;

        InversaA, InversaB, InversaC, InversaD, InversaE, InversaF, InversaG, InversaH, InversaI, InversaJ, InversaK, InversaL: Matriz;

        DeterminanteA, DeterminanteB, DeterminanteC, DeterminanteD, DeterminanteE, DeterminanteF, DeterminanteG, DeterminanteH, DeterminanteI, DeterminanteJ, DeterminanteK, DeterminanteL: Real;

        Escalar: Real;

        NLinhasA, NColunasA, NLinhasB, NColunasB, NLinhasC, NColunasC, NLinhasD, NColunasD, NLinhasE, NColunasE, NLinhasF, NColunasF, NLinhasG, NColunasG, NLinhasH, NColunasH, NLinhasI, NColunasI, NLinhasJ, NColunasJ, NLinhasK, NColunasK, NLinhasL, NColunasL: Inteiro;

    begin

        NLinhasA := 3;

        NColunasA := 3;

        A := [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

        NLinhasB := 3;

        NColunasB := 3;

        B := [[10, 11, 12], [13, 14, 15], [16, 17, 18]];

        NLinhasC := 3;

        NColunasC := 3;

        C := [[19, 20, 21], [22, 23, 24], [25, 26, 27]];

        NLinhasD := 3;

        NColunasD := 3;

        D := [[28, 29, 30], [31, 32, 33], [34, 35, 36]];

        NLinhasE := 3;

        NColunasE := 3;

        E := [[37, 38, 39], [40, 41, 42], [43, 44, 45]];

        NLinhasF := 3;

        NColunasF := 3;

        F := [[46, 47, 48], [49, 50, 51], [52, 53, 54]];

        NLinhasG := 3;

        NColunasG := 3;

        G := [[55, 56, 57], [58, 59, 60], [61, 62, 63]];

        NLinhasH := 3;

        NColunasH := 3;

        H := [[64, 65, 66], [67, 68, 69], [70, 71, 72]];

        NLinhasI := 3;

        NColunasI := 3;

        I := [[73, 74, 75], [76, 77, 78], [79, 80, 81]];

        NLinhasJ := 3;

        NColunasJ := 3;

        J := [[82, 83, 84], [85, 86, 87], [88, 89, 90]];

        NLinhasK := 3;

        NColunasK := 3;

        K := [[91, 92, 93], [94, 95, 96], [97, 98, 99]];

        NLinhasL := 3;

        NColunasL := 3;

        L := [[100, 101, 102], [103, 104, 105], [106, 107, 108]];

        TranspostaA := TranspostaMatriz(A, NLinhasA, NColunasA);

        TranspostaB := TranspostaMatriz(B, NLinhasB, NColunasB);

        TranspostaC := TranspostaMatriz(C, NLinhasC, NColunasC);

        TranspostaD := TranspostaMatriz(D, NLinhasD, NColunasD);

        TranspostaE := TranspostaMatriz(E, NLinhasE, NColunasE);

        TranspostaF := TranspostaMatriz(F, NLinhasF, NColunasF);

        TranspostaG := TranspostaMatriz(G, NLinhasG, NColunasG);

        TranspostaH := TranspostaMatriz(H, NLinhasH, NColunasH);

        TranspostaI := TranspostaMatriz(I, NLinhasI, NColunasI);

        TranspostaJ := TranspostaMatriz(J, NLinhasJ, NColunasJ);

        TranspostaK := TranspostaMatriz(K, NLinhasK, NColunasK);

        TranspostaL := Trans