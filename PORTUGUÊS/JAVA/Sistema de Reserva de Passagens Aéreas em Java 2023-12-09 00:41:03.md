Claro! Aqui está um código em Java que realiza uma simulação de um sistema de reserva de passagens aéreas. O código é complexo e abrange várias classes e funcionalidades. 

Primeiro, criaremos uma classe chamada "Passageiro" que representa um passageiro com nome, idade e número de telefone. Faremos uso de encapsulamento para proteger os dados do passageiro:

```java
public class Passageiro {
    private String nome;
    private int idade;
    private String telefone;

    public Passageiro(String nome, int idade, String telefone) {
        this.nome = nome;
        this.idade = idade;
        this.telefone = telefone;
    }

    // getters e setters
    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public int getIdade() {
        return idade;
    }

    public void setIdade(int idade) {
        this.idade = idade;
    }

    public String getTelefone() {
        return telefone;
    }

    public void setTelefone(String telefone) {
        this.telefone = telefone;
    }
}
```

Em seguida, criaremos uma classe chamada "Voo" que representa um voo com número, origem, destino, data e horário. Nesta classe, também faremos uso de encapsulamento:

```java
public class Voo {
    private String numero;
    private String origem;
    private String destino;
    private String data;
    private String horario;

    public Voo(String numero, String origem, String destino, String data, String horario) {
        this.numero = numero;
        this.origem = origem;
        this.destino = destino;
        this.data = data;
        this.horario = horario;
    }

    // getters e setters
    public String getNumero() {
        return numero;
    }

    public void setNumero(String numero) {
        this.numero = numero;
    }

    public String getOrigem() {
        return origem;
    }

    public void setOrigem(String origem) {
        this.origem = origem;
    }

    public String getDestino() {
        return destino;
    }

    public void setDestino(String destino) {
        this.destino = destino;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public String getHorario() {
        return horario;
    }

    public void setHorario(String horario) {
        this.horario = horario;
    }
}
```

Agora, criaremos uma classe chamada "Reserva" que representa uma reserva de passagem aérea. Esta classe possui um passageiro, um voo e um status (reservado ou cancelado):

```java
public class Reserva {
    private Passageiro passageiro;
    private Voo voo;
    private String status;

    public Reserva(Passageiro passageiro, Voo voo) {
        this.passageiro = passageiro;
        this.voo = voo;
        this.status = "Reservado";
    }

    public void cancelarReserva() {
        this.status = "Cancelado";
    }

    // getters e setters
    public Passageiro getPassageiro() {
        return passageiro;
    }

    public void setPassageiro(Passageiro passageiro) {
        this.passageiro = passageiro;
    }

    public Voo getVoo() {
        return voo;
    }

    public void setVoo(Voo voo) {
        this.voo = voo;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }
}
```

Por fim, criaremos uma classe chamada "SistemaReservas" que realizará a simulação do sistema de reserva de passagens aéreas. Nesta classe, serão criados alguns passageiros, voos e reservas, além de ser possível cancelar uma reserva:

```java
import java.util.ArrayList;
import java.util.List;

public class SistemaReservas {
    public static void main(String[] args) {
        // Criando passageiros
        Passageiro passageiro1 = new Passageiro("João Silva", 25, "987654321");
        Passageiro passageiro2 = new Passageiro("Maria Santos", 30, "123456789");

        // Criando voos
        Voo voo1 = new Voo("123", "São Paulo", "Rio de Janeiro", "01/01/2022", "10:00");
        Voo voo2 = new Voo("456", "Rio de Janeiro", "São Paulo", "02/01/2022", "15:00");

        // Criando reservas
        Reserva reserva1 = new Reserva(passageiro1, voo1);
        Reserva reserva2 = new Reserva(passageiro2, voo2);

        // Adicionando reservas a uma lista de reservas
        List<Reserva> reservas = new ArrayList<>();
        reservas.add(reserva1);
        reservas.add(reserva2);

        // Imprimindo informações das reservas
        for (Reserva reserva : reservas) {
            System.out.println("Passageiro: " + reserva.getPassageiro().getNome());
            System.out.println("Voo: " + reserva.getVoo().getNumero());
            System.out.println("Status: " + reserva.getStatus());
            System.out.println("----------------");
        }

        // Cancelando a primeira reserva
        reserva1.cancelarReserva();

        // Imprimindo informações atualizadas das reservas
        for (Reserva reserva : reservas) {
            System.out.println("Passageiro: " + reserva.getPassageiro().getNome());
            System.out.println("Voo: " + reserva.getVoo().getNumero());
            System.out.println("Status: " + reserva.getStatus());
            System.out.println("----------------");
        }
    }
}
```

Este código em Java simula um sistema de reserva de passagens aéreas, permitindo criar passageiros, voos e reservas, além de cancelar uma reserva. Ele utiliza classes, encapsulamento e uma lista para armazenar as reservas.