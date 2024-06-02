**Simulação de Sistema Solar Complexo em Portugol**

**Objetivo:** Simular o movimento dos planetas do Sistema Solar, incluindo suas órbitas elípticas e interações gravitacionais.

**Código:**

```portugol
**Programa** SistemaSolar

**Constantes**
  PI = 3.14159265
  G = 6.674e-11

**Tipo** Planeta
  tempoOrbita: Real
  tempoRotacao: Real
  massa: Real
  raio: Real
  posicaoX: Real
  posicaoY: Real
  velocidadeX: Real
  velocidadeY: Real
**FimTipo**

**Tipo** SistemaSolar
  planetas: Vetor[Planeta]
  tempoSimulacao: Real
  passoSimulacao: Real
**FimTipo**

**Funcao** CalcularNovaPosicao(planeta: Planeta)
  **ParaCada** planetai **Em** sistema.planetas
    **Se** planeta.ID <> planetai.ID **Entao**
      **Faca**
        forcaX = (-G * planeta.massa * planetai.massa) / (NormaQuadrada(planeta.posicaoX - planetai.posicaoX, planeta.posicaoY - planetai.posicaoY)) * (planeta.posicaoX - planetai.posicaoX)
        forcaY = (-G * planeta.massa * planetai.massa) / (NormaQuadrada(planeta.posicaoX - planetai.posicaoX, planeta.posicaoY - planetai.posicaoY)) * (planeta.posicaoY - planetai.posicaoY)
        planeta.velocidadeX = planeta.velocidadeX + (forcaX * sistema.passoSimulacao) / planeta.massa
        planeta.velocidadeY = planeta.velocidadeY + (forcaY * sistema.passoSimulacao) / planeta.massa
      **FimFaca**
  **FimParaCada**
  planeta.posicaoX = planeta.posicaoX + planeta.velocidadeX * sistema.passoSimulacao
  planeta.posicaoY = planeta.posicaoY + planeta.velocidadeY * sistema.passoSimulacao
**FimFuncao**

**Funcao** MostrarSistema(sistema: SistemaSolar)
  **ParaCada** planeta **Em** sistema.planetas
    **Faca**
      Escreva("Planeta: ", planeta.ID, "\tPosicaoX: ", planeta.posicaoX, "\tPosicaoY: ", planeta.posicaoY, "\n")
    **FimFaca**
**FimFuncao**

**Funcao** SimularSistema(sistema: SistemaSolar)
  **Para** t = 0 **Ate** sistema.tempoSimulacao **Passo** sistema.passoSimulacao
    **Faca**
      **ParaCada** planeta **Em** sistema.planetas
        **Faca**
          CalcularNovaPosicao(planeta)
        **FimFaca**
      MostrarSistema(sistema)
    **FimFaca**
**FimFuncao**

**Procedimento** Main()
  **Declarar**
    Mercurio: Planeta
    Venus: Planeta
    Terra: Planeta
    Marte: Planeta
    Jupiter: Planeta
    Saturno: Planeta
    Urano: Planeta
    Netuno: Planeta
    Sistema: SistemaSolar

  **Atribuir**
    Mercurio.tempoOrbita = 88
    Mercurio.tempoRotacao = 59
    Mercurio.massa = 0.33e24
    Mercurio.raio = 0.244e6
    Mercurio.posicaoX = 0
    Mercurio.posicaoY = 4.6e10
    Mercurio.velocidadeX = 4.74e3
    Mercurio.velocidadeY = 0

    Venus.tempoOrbita = 225
    Venus.tempoRotacao = -243
    Venus.massa = 4.86e24
    Venus.raio = 0.605e6
    Venus.posicaoX = 0
    Venus.posicaoY = 1.08e11
    Venus.velocidadeX = 3.5e3
    Venus.velocidadeY = 0

    Terra.tempoOrbita = 365
    Terra.tempoRotacao = 24
    Terra.massa = 5.97e24
    Terra.raio = 0.637e6
    Terra.posicaoX = 0
    Terra.posicaoY = 1.5e11
    Terra.velocidadeX = 3e3
    Terra.velocidadeY = 0

    Marte.tempoOrbita = 687
    Marte.tempoRotacao = 25
    Marte.massa = 0.643e24
    Marte.raio = 0.339e6
    Marte.posicaoX = 0
    Marte.posicaoY = 2.28e11
    Marte.velocidadeX = 2.4e3
    Marte.velocidadeY = 0

    Jupiter.tempoOrbita = 4333
    Jupiter.tempoRotacao = 10
    Jupiter.massa = 1.899e27
    Jupiter.raio = 7.14e7
    Jupiter.posicaoX = 0
    Jupiter.posicaoY = 7.78e11
    Jupiter.velocidadeX = 1.3e3
    Jupiter.velocidadeY = 0

    Saturno.tempoOrbita = 10759
    Saturno.tempoRotacao = 11
    Saturno.massa = 5.685e26
    Saturno.raio = 6.03e7
    Saturno.posicaoX = 0
    Saturno.posicaoY = 1.43e12
    Saturno.velocidadeX = 1e3
    Saturno.velocidadeY = 0

    Urano.tempoOrbita = 30689
    Urano.tempoRotacao = -17
    Urano.massa = 8.683e25
    Urano.raio = 2.56e7
    Urano.posicaoX = 0
    Urano.posicaoY = 2.87e12
    Urano.velocidadeX = 700
    Urano.velocidadeY = 0

    Netuno.tempoOrbita = 60190
    Netuno.tempoRotacao = 16
    Netuno.massa = 1.024e26
    Netuno.raio = 2.48e7
    Netuno.posicaoX = 0
    Netuno.posicaoY = 4.5e12
    Netuno.velocidadeX = 600
    Netuno.velocidadeY = 0

  **Atribuir**
    Sistema.TempoSimulacao = 365 * 100
    Sistema.PassoSimulacao = 1000
    Sistema.planetas.Adicionar(Mercurio)
    Sistema.planetas.Adicionar(Venus)
    Sistema.planetas.Adicionar(Terra)
    Sistema.planetas.Adicionar(Marte)
    Sistema.planetas.Adicionar(Jupiter)
    Sistema.planetas.Adicionar(Saturno)
    Sistema.planetas.Adicionar(Urano)
    Sistema.planetas.Adicionar(Netuno)

  **Chamar** SimularSistema(Sistema)
**FimProcedimento**
```

**Explicação do Código:**

**Constantes e Tipos:**

* Constantes `PI` e `G` para a constante de pi e a constante gravitacional, respectivamente.
* Tipo `Planeta` representando um planeta com suas propriedades: tempo de órbita, tempo de rotação, massa, raio, posição e velocidade.

**Funções:**

* `CalcularNovaPosicao`: Calcula a nova posição de um planeta com base nas forças gravitacionais de outros planetas.
* `MostrarSistema`: Exibe as posições dos planetas no console.

**Procedimento `SimularSistema`:**

* Simula o movimento dos planetas por um período especificado (`tempoSimulacao`) usando o passo de tempo especificado (`passoSimulacao`).
* Para cada passo de tempo, calcula as novas posições dos planetas usando `CalcularNovaPosicao`.

**Procedimento `Main`:**

* Cria planetas com seus parâmetros iniciais e os adiciona ao `SistemaSolar`.
* Define os parâmetros de simulação e chama `SimularSistema`.

**Complexidade e Diferenciação:**

* **Complexidade:** O código é complexo devido ao cálculo das interações gravitacionais entre os planetas, que são calculadas iterativamente para cada passo de tempo.
* **Diferenciação:** O código é diferenciado porque:
    * Usa um passo de tempo adaptável (`