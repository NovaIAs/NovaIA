Claro! Aqui está um exemplo de um código Java complexo que usa conceitos avançados de programação:

```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ProcessadorDeDados {

    public static void main(String[] args) {
        List<Registro> registros = criarRegistros();

        Map<String, List<Registro>> registrosPorCategoria = agruparRegistrosPorCategoria(registros);

        imprimirRegistrosPorCategoria(registrosPorCategoria);
    }

    private static List<Registro> criarRegistros() {
        List<Registro> registros = new ArrayList<>();

        registros.add(new Registro("João", "CategoriaA", 100));
        registros.add(new Registro("Maria", "CategoriaB", 200));
        registros.add(new Registro("Pedro", "CategoriaA", 150));
        registros.add(new Registro("Ana", "CategoriaC", 300));
        registros.add(new Registro("Carlos", "CategoriaA", 50));
        registros.add(new Registro("Mariana", "CategoriaB", 250));

        return registros;
    }

    private static Map<String, List<Registro>> agruparRegistrosPorCategoria(List<Registro> registros) {
        Map<String, List<Registro>> registrosPorCategoria = new HashMap<>();

        for (Registro registro : registros) {
            String categoria = registro.getCategoria();

            if (!registrosPorCategoria.containsKey(categoria)) {
                registrosPorCategoria.put(categoria, new ArrayList<>());
            }

            registrosPorCategoria.get(categoria).add(registro);
        }

        return registrosPorCategoria;
    }

    private static void imprimirRegistrosPorCategoria(Map<String, List<Registro>> registrosPorCategoria) {
        for (Map.Entry<String, List<Registro>> entry : registrosPorCategoria.entrySet()) {
            String categoria = entry.getKey();
            List<Registro> registros = entry.getValue();

            System.out.println("Categoria: " + categoria);
            System.out.println("-------------");

            for (Registro registro : registros) {
                System.out.println("Nome: " + registro.getNome());
                System.out.println("Valor: " + registro.getValor());
                System.out.println("-------------");
            }
        }
    }

    private static class Registro {
        private String nome;
        private String categoria;
        private int valor;

        public Registro(String nome, String categoria, int valor) {
            this.nome = nome;
            this.categoria = categoria;
            this.valor = valor;
        }

        public String getNome() {
            return nome;
        }

        public String getCategoria() {
            return categoria;
        }

        public int getValor() {
            return valor;
        }
    }
}
```

Neste exemplo, temos uma classe `ProcessadorDeDados` que contém um método `main`. Neste método, criamos uma lista de `Registros` usando o método `criarRegistros`. 

Em seguida, utilizamos o método `agruparRegistrosPorCategoria` para agrupar os registros por categoria, armazenando os resultados em um `Map` chamado `registrosPorCategoria`.

Por fim, chamamos o método `imprimirRegistrosPorCategoria` para exibir os registros agrupados por categoria no console.

A classe `Registro` é uma classe interna que representa um registro com nome, categoria e valor.