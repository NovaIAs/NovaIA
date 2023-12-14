Claro! Aqui está um código complexo em D que demonstra o uso de classes, herança, polimorfismo e interfaces:

```d
import std.stdio;

// Classe base Veiculo
class Veiculo {
    protected string modelo;
    protected int anoFabricacao;

    this(string modelo, int anoFabricacao) {
        this.modelo = modelo;
        this.anoFabricacao = anoFabricacao;
    }

    void acelerar() {
        writeln("O ", this.modelo, " está acelerando...");
    }

    void frear() {
        writeln("O ", this.modelo, " está freando...");
    }
}

// Classe Carro que herda de Veiculo
class Carro : Veiculo {
    private int numeroPortas;

    this(string modelo, int anoFabricacao, int numeroPortas) {
        super(modelo, anoFabricacao);
        this.numeroPortas = numeroPortas;
    }

    override void acelerar() {
        writeln("O ", this.modelo, " está acelerando rapidamente!");
    }

    override void frear() {
        writeln("O ", this.modelo, " está freando bruscamente!");
    }

    void abrirPortas() {
        writeln("Abrindo as ", this.numeroPortas, " portas do ", this.modelo);
    }
}

// Classe Moto que herda de Veiculo
class Moto : Veiculo {
    private bool temPartidaEletrica;

    this(string modelo, int anoFabricacao, bool temPartidaEletrica) {
        super(modelo, anoFabricacao);
        this.temPartidaEletrica = temPartidaEletrica;
    }

    override void acelerar() {
        writeln("A ", this.modelo, " está acelerando na estrada!");
    }

    override void frear() {
        writeln("A ", this.modelo, " está freando suavemente!");
    }

    void ligarPartidaEletrica() {
        if (this.temPartidaEletrica) {
            writeln("Ligando a partida elétrica da ", this.modelo);
        } else {
            writeln("A ", this.modelo, " não possui partida elétrica.");
        }
    }
}

// Interface para veículos com tração nas quatro rodas
interface QuatroRodas {
    bool temTracaoQuatroRodas();
}

// Classe SUV que herda de Carro e implementa a interface QuatroRodas
class SUV : Carro, QuatroRodas {
    this(string modelo, int anoFabricacao, int numeroPortas) {
        super(modelo, anoFabricacao, numeroPortas);
    }

    bool temTracaoQuatroRodas() {
        return true;
    }
}

void main() {
    // Criando instâncias dos veículos
    Carro carro = new Carro("Fiesta", 2019, 4);
    Moto moto = new Moto("CG 125", 2020, false);
    SUV suv = new SUV("Ecosport", 2018, 4);

    // Chamando os métodos dos veículos
    carro.acelerar();
    carro.frear();
    carro.abrirPortas();
    writeln();

    moto.acelerar();
    moto.frear();
    moto.ligarPartidaEletrica();
    writeln();

    suv.acelerar();
    suv.frear();
    suv.abrirPortas();
    writeln("Possui tração nas quatro rodas? ", suv.temTracaoQuatroRodas());
}
```

Neste código, temos a classe base `Veiculo` que possui os atributos `modelo` e `anoFabricacao`, bem como os métodos `acelerar()` e `frear()`. Em seguida, temos a classe `Carro` que herda de `Veiculo`, adicionando o atributo `numeroPortas` e sobrescrevendo os métodos `acelerar()` e `frear()`. A classe `Moto` também herda de `Veiculo`, mas adiciona o atributo `temPartidaEletrica` e sobrescreve os métodos `acelerar()` e `frear()`. Além disso, temos a interface `QuatroRodas` com o método `temTracaoQuatroRodas()`, que é implementado pela classe `SUV`, que herda de `Carro`.

No `main()`, criamos instâncias dos veículos e chamamos seus respectivos métodos. O código exemplifica a utilização de classes, herança, polimorfismo e interfaces em um cenário de veículos.