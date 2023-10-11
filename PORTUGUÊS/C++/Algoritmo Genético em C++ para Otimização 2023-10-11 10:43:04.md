Claro! Aqui está um código complexo em C++ que implementa um algoritmo de otimização conhecido como Algoritmo Genético:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <chrono>

// Definição do tamanho da população e do número de gerações
const int TAMANHO_POPULACAO = 100;
const int NUMERO_GERACOES = 100;
const double TAXA_MUTACAO = 0.1;

// Classe que representa um indivíduo
class Individuo {
public:
    // Construtor
    Individuo(int tamanhoCromossomo) : cromossomo(tamanhoCromossomo), aptidao(0) {
        // Inicializa o cromossomo com valores aleatórios
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<> dis(0.0, 1.0);
        for (int i = 0; i < tamanhoCromossomo; i++) {
            cromossomo[i] = dis(gen);
        }
    }

    // Calcula a aptidão do indivíduo
    void calcularAptidao() {
        // Aqui você deve implementar a função de aptidão do problema específico
        // Quanto maior a aptidão, melhor é o indivíduo
        // Neste exemplo, a aptidão é a soma dos valores do cromossomo
        aptidao = 0;
        for (int i = 0; i < cromossomo.size(); i++) {
            aptidao += cromossomo[i];
        }
    }

    // Operador de cruzamento entre dois indivíduos
    Individuo cruzarCom(const Individuo& outro) const {
        Individuo filho(cromossomo.size());
        for (int i = 0; i < cromossomo.size(); i++) {
            filho.cromossomo[i] = (cromossomo[i] + outro.cromossomo[i]) / 2;
        }
        return filho;
    }

    // Operador de mutação
    void mutar() {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<> dis(0.0, 1.0);

        for (int i = 0; i < cromossomo.size(); i++) {
            if (dis(gen) < TAXA_MUTACAO) {
                cromossomo[i] = dis(gen);
            }
        }
    }

    // Getter para acessar o cromossomo
    std::vector<double> getCromossomo() const {
        return cromossomo;
    }

    // Getter para acessar a aptidão
    double getAptidao() const {
        return aptidao;
    }

private:
    std::vector<double> cromossomo;
    double aptidao;
};

// Função para ordenar a população em ordem decrescente de aptidão
bool compararPorAptidao(const Individuo& a, const Individuo& b) {
    return a.getAptidao() > b.getAptidao();
}

// Função para evoluir a população por uma geração
void evoluir(std::vector<Individuo>& populacao) {
    // Ordena a população em ordem decrescente de aptidão
    std::sort(populacao.begin(), populacao.end(), compararPorAptidao);

    // Cria uma nova população vazia
    std::vector<Individuo> novaPopulacao(TAMANHO_POPULACAO);

    // Elitismo: mantém os 10% melhores indivíduos da população atual
    int elitismo = TAMANHO_POPULACAO * 0.1;
    for (int i = 0; i < elitismo; i++) {
        novaPopulacao[i] = populacao[i];
    }

    // Cruzamento: preenche o restante da nova população com filhos de indivíduos selecionados aleatoriamente
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, elitismo - 1);
    for (int i = elitismo; i < TAMANHO_POPULACAO; i++) {
        int pai1Index = dis(gen);
        int pai2Index = dis(gen);
        Individuo pai1 = populacao[pai1Index];
        Individuo pai2 = populacao[pai2Index];
        Individuo filho = pai1.cruzarCom(pai2);
        filho.mutar();
        novaPopulacao[i] = filho;
    }

    // Atualiza a população
    populacao = novaPopulacao;
}

int main() {
    // Inicializa a população com indivíduos aleatórios
    std::vector<Individuo> populacao(TAMANHO_POPULACAO, Individuo(10));

    // Avalia a aptidão de cada indivíduo
    for (int i = 0; i < TAMANHO_POPULACAO; i++) {
        populacao[i].calcularAptidao();
    }

    // Evolui a população por um número fixo de gerações
    for (int geracao = 0; geracao < NUMERO_GERACOES; geracao++) {
        evoluir(populacao);

        // Exibe a melhor aptidão da geração atual
        std::cout << "Geração " << geracao << ": Melhor aptidão = " << populacao[0].getAptidao() << std::endl;
    }

    // Exibe o melhor indivíduo da última geração
    std::cout << "Melhor indivíduo: ";
    std::vector<double> melhorCromossomo = populacao[0].getCromossomo();
    for (int i = 0; i < melhorCromossomo.size(); i++) {
        std::cout << melhorCromossomo[i] << " ";
    }
    std::cout << std::endl;

    return 0;
}
```

Neste código, implementei um algoritmo genético básico que pode ser utilizado para resolver problemas de otimização. A população inicial é composta por indivíduos com cromossomos aleatórios. A aptidão de cada indivíduo é calculada através de uma função de aptidão específica do problema em questão. A cada geração, os indivíduos são selecionados para cruzamento e mutação, gerando uma nova população. No final do algoritmo, é exibido o melhor indivíduo encontrado.

É importante ressaltar que este código é apenas um exemplo básico de implementação de algoritmo genético e pode ser adaptado para diferentes problemas. O desenvolvimento de uma função de aptidão adequada e a escolha de parâmetros adequados são fundamentais para obter bons resultados no algoritmo.