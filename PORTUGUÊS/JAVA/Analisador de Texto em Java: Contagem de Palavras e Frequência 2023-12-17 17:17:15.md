Claro! Aqui está um código complexo em Java que envolve manipulação de arquivos, tratamento de exceções e uso de estruturas de dados avançadas:

```java
import java.io.*;
import java.util.*;

public class AnalisadorDeTexto {

    public static void main(String[] args) {
        try {
            // Leitura do arquivo de entrada
            File arquivoEntrada = new File("texto.txt");
            BufferedReader leitor = new BufferedReader(new FileReader(arquivoEntrada));
            
            // Criação do arquivo de saída
            File arquivoSaida = new File("resultado.txt");
            BufferedWriter escritor = new BufferedWriter(new FileWriter(arquivoSaida));
            
            // Contadores de palavras
            int totalPalavras = 0;
            Map<String, Integer> frequenciaPalavras = new TreeMap<>();
            
            // Leitura e processamento do texto
            String linha;
            while ((linha = leitor.readLine()) != null) {
                String[] palavras = linha.split("\\s+");
                totalPalavras += palavras.length;
                
                for (String palavra : palavras) {
                    palavra = palavra.toLowerCase();
                    if (frequenciaPalavras.containsKey(palavra)) {
                        int frequencia = frequenciaPalavras.get(palavra);
                        frequenciaPalavras.put(palavra, frequencia + 1);
                    } else {
                        frequenciaPalavras.put(palavra, 1);
                    }
                }
            }
            
            // Escrita do resultado
            escritor.write("Total de palavras: " + totalPalavras);
            escritor.newLine();
            
            for (Map.Entry<String, Integer> entrada : frequenciaPalavras.entrySet()) {
                escritor.write(entrada.getKey() + ": " + entrada.getValue());
                escritor.newLine();
            }
            
            // Fechamento dos arquivos
            leitor.close();
            escritor.close();
            
            System.out.println("Análise concluída. Resultados salvos em resultado.txt.");
        } catch (FileNotFoundException e) {
            System.out.println("Arquivo de entrada não encontrado.");
        } catch (IOException e) {
            System.out.println("Erro ao ler/escrever no arquivo.");
        }
    }
}
```

Este código realiza a análise de um arquivo de texto chamado "texto.txt", contando o número total de palavras e exibindo a frequência de cada palavra no arquivo. Os resultados são salvos no arquivo "resultado.txt".

A primeira parte do código é responsável pela leitura do arquivo de entrada e criação do arquivo de saída. Em seguida, temos a declaração de variáveis para armazenar o total de palavras e a frequência de cada palavra.

O texto é lido linha por linha e cada linha é dividida em palavras usando o método `split()` do objeto `String`. O número de palavras em cada linha é somado ao contador `totalPalavras` e, em seguida, cada palavra é convertida para letras minúsculas e adicionada ao mapa `frequenciaPalavras`. Se a palavra já existe no mapa, sua frequência é incrementada; caso contrário, a palavra é adicionada ao mapa com frequência 1.

Após processar todo o texto, os resultados são escritos no arquivo de saída. Primeiro, é escrito o total de palavras e, em seguida, cada palavra e sua frequência são escritas em linhas separadas.

Por fim, os arquivos são fechados e uma mensagem de conclusão é exibida.

Espero que este código atenda às suas expectativas!