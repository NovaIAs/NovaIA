Claro! Aqui está um código complexo em Kotlin que utiliza a biblioteca Ktor para criar um servidor HTTP e manipular requisições e respostas:

```kotlin
import io.ktor.application.*
import io.ktor.features.ContentNegotiation
import io.ktor.features.StatusPages
import io.ktor.http.HttpStatusCode
import io.ktor.jackson.jackson
import io.ktor.request.receive
import io.ktor.response.respond
import io.ktor.routing.*
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty

fun main() {
    embeddedServer(Netty, port = 8080, module = Application::module).start(wait = true)
}

fun Application.module() {
    install(ContentNegotiation) {
        jackson {}
    }

    install(StatusPages) {
        exception<Throwable> { cause ->
            call.respond(HttpStatusCode.InternalServerError, "Erro interno no servidor: ${cause.localizedMessage}")
        }
    }

    routing {
        route("/api") {
            get("/hello") {
                call.respond(HttpStatusCode.OK, "Olá, mundo!")
            }

            post("/user") {
                val user = call.receive<User>()
                // Lógica para salvar o usuário no banco de dados ou realizar outras operações

                call.respond(HttpStatusCode.Created, "Usuário ${user.name} criado com sucesso!")
            }
        }
    }
}

data class User(val name: String, val age: Int)
```

Neste código, utilizamos o framework Ktor para criar um servidor HTTP. Começamos importando as bibliotecas necessárias e definindo o ponto de entrada da aplicação na função `main()`, onde criamos e iniciamos o servidor.

Em seguida, na função `module()` definimos as configurações principais da aplicação. Instalamos o `ContentNegotiation`, que permite a serialização e desserialização automática de objetos JSON utilizando a biblioteca Jackson. Também instalamos o `StatusPages`, que trata exceções e retorna respostas adequadas.

Dentro do `routing`, definimos as rotas do servidor. Na rota `/api/hello` retornamos a resposta "Olá, mundo!" quando a requisição HTTP for do tipo GET. Já na rota `/api/user` esperamos uma requisição POST contendo um objeto `User` no corpo da requisição. Utilizamos a função `call.receive<User>()` para desserializar o objeto e podemos então realizar alguma lógica com ele, como salvar em um banco de dados. Por fim, retornamos a resposta "Usuário {nome} criado com sucesso!" com o código de status 201 (Created).

Espero que esse código atenda às suas expectativas!