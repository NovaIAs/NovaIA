Claro! Aqui está um código em Kotlin que implementa um sistema de reservas de hotéis:

```kotlin
import java.util.*

class Hotel(val nome: String, val localizacao: String, val capacidade: Int) {
    var quartosDisponiveis: Int = capacidade
        private set

    var reservas: MutableList<Reserva> = mutableListOf()

    fun fazerReserva(dataInicio: Date, dataFim: Date, numeroPessoas: Int): Reserva? {
        if (numeroPessoas <= quartosDisponiveis && dataInicio.before(dataFim)) {
            val reserva = Reserva(dataInicio, dataFim, numeroPessoas)
            reservas.add(reserva)
            quartosDisponiveis -= numeroPessoas
            return reserva
        }
        return null
    }

    fun cancelarReserva(reserva: Reserva) {
        if (reservas.remove(reserva)) {
            quartosDisponiveis += reserva.numeroPessoas
        }
    }
}

class Reserva(val dataInicio: Date, val dataFim: Date, val numeroPessoas: Int)

fun main() {
    val hotel = Hotel("Hotel Exemplo", "Cidade Exemplo", 100)

    val dataInicio = Calendar.getInstance()
    dataInicio.set(2022, Calendar.JANUARY, 1)

    val dataFim = Calendar.getInstance()
    dataFim.set(2022, Calendar.JANUARY, 5)

    val reserva = hotel.fazerReserva(dataInicio.time, dataFim.time, 2)

    if (reserva != null) {
        println("Reserva realizada com sucesso!")
        println("Data de início: ${reserva.dataInicio}")
        println("Data de fim: ${reserva.dataFim}")
        println("Número de pessoas: ${reserva.numeroPessoas}")
    } else {
        println("Não foi possível realizar a reserva.")
    }
}
```

Neste código, temos a classe `Hotel`, que representa um hotel com um nome, localização e capacidade de quartos. O número de quartos disponíveis é atualizado sempre que uma reserva é feita ou cancelada. A classe `Reserva` representa uma reserva de hotel, com data de início, data de fim e número de pessoas.

Na função `main`, iniciamos um exemplo de hotel e criamos um objeto `Calendar` para representar as datas de início e fim da reserva. Em seguida, fazemos uma reserva chamando o método `fazerReserva` do objeto `hotel` e passando as datas e o número de pessoas desejadas. Se a reserva for bem-sucedida, imprimimos os detalhes da reserva. Caso contrário, exibimos uma mensagem de erro.

Espero que esse código atenda às suas expectativas!