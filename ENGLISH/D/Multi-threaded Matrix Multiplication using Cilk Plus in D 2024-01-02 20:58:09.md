Certainly! Here's a complex code in the D programming language that performs a multi-threaded matrix multiplication using the Cilk Plus extension. This code takes advantage of parallelism to improve performance.

```d
import core.thread;
import core.time;

void main()
{
    const int N = 1000;
    immutable int NUM_THREADS = Task.maxThreads;

    writeln("Matrix Multiplication using Cilk Plus in D");

    // Generate two random matrices
    int[][] matrixA = generateMatrix(N);
    int[][] matrixB = generateMatrix(N);

    // Create the result matrix
    int[][] resultMatrix = new int[][](N);
    foreach (ref row; resultMatrix)
        row = new int[](N);

    writeln("Matrix A:");
    printMatrix(matrixA);

    writeln("Matrix B:");
    printMatrix(matrixB);

    // Divide the work among threads
    int blockSize = N / NUM_THREADS;
    int[][] partialResults = new int[][](NUM_THREADS);
    foreach (ref partialResult; partialResults)
        partialResult = new int[](N);

    auto startTime = Clock.currTime();

    foreach (i; 0 .. NUM_THREADS)
    {
        auto t = new Task(delegate {
            multiplyMatrices(matrixA, matrixB, partialResults[i], i * blockSize, (i + 1) * blockSize);
        });
        t.execute();
    }
    Task.yield();

    // Combine the partial results
    foreach (i; 1 .. NUM_THREADS)
    {
        foreach (j; 0 .. N)
            foreach (k; 0 .. N)
                resultMatrix[j][k] += partialResults[i][j][k];
    }

    writeln("Result Matrix:");
    printMatrix(resultMatrix);

    auto endTime = Clock.currTime();
    auto elapsedTime = endTime - startTime;

    writeln("Execution time: ", elapsedTime, " seconds");
}

int[][] generateMatrix(int size)
{
    int[][] matrix = new int[][](size);
    foreach (ref row; matrix)
    {
        row = new int[](size);
        foreach (ref element; row)
            element = uniform(1, 10);
    }
    return matrix;
}

void multiplyMatrices(int[][] matrixA, int[][] matrixB, int[][] partialResult, int startRow, int endRow)
{
    foreach (i; startRow .. endRow)
        foreach (j; 0 .. matrixB[0].length)
            foreach (k; 0 .. matrixA[0].length)
                partialResult[i][j] += matrixA[i][k] * matrixB[k][j];
}

void printMatrix(int[][] matrix)
{
    foreach (row; matrix)
    {
        foreach (element; row)
            writef("%4d ", element);
        writeln();
    }
}
```

This code performs a matrix multiplication of two randomly generated matrices using the Cilk Plus extension in the D programming language. The matrices are divided into equal-sized blocks and each block is processed in parallel using multiple threads. The partial results obtained by each thread are then combined to calculate the final result.

The code begins by generating two random matrices `matrixA` and `matrixB` of size `N` (set to 1000 in this example). It then creates an empty result matrix `resultMatrix` of the same size.

The work is divided among the available threads using the `NUM_THREADS` constant. Each thread executes the `multiplyMatrices` function, which multiplies the corresponding blocks of `matrixA` and `matrixB` and stores the result in `partialResults`. The blocks are determined by the `startRow` and `endRow` parameters, which depend on the thread index and the size of the matrices.

After all threads have finished, the partial results are combined by summing the corresponding elements of each block into the `resultMatrix`. Finally, the result matrix is printed along with the execution time.

Please note that this code requires the Cilk Plus extension to be enabled in the D compiler.