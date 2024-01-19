```scala
// This is a Scala program that implements a simple web server.

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, HttpRequest}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

object WebServer extends App with ActorLogging {

  // The port on which the server will listen for requests.
  private val port = 8080

  // The actor system that will manage the server and its actors.
  private val system = ActorSystem("webserver")

  // The materializer that will be used to handle HTTP requests and responses.
  private val materializer = ActorMaterializer()

  // The actor that will handle HTTP requests and responses.
  private val handler = system.actorOf(Props[RequestHandler], "requestHandler")

  // The route that will be used to handle HTTP requests.
  private val route = pathSingleSlash {
    get {
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Hello, world!"))
    }
  } ~ path("echo") {
    post {
      entity(as[String]) { message =>
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"Echo: $message"))
      }
    }
  }

  // Start the server.
  Http().bindAndHandle(route, "localhost", port, materializer)

  // Log a message when the server has started.
  log.info(s"Server is listening on port $port")

  // Stop the actor system when the program exits.
  sys.addShutdownHook(() => system.terminate())
}

// The actor that will handle HTTP requests and responses.
class RequestHandler extends Actor with ActorLogging {

  // The method that will be called when the actor receives a message.
  def receive = {
    case HttpRequest(method, uri, headers, entity, protocol) =>
      log.info(s"Received request: $method $uri")

      // Send a response back to the client.
      sender ! HttpResponse(entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "Hello, world!"))
  }
}
```

This code implements a simple web server in Scala using the Akka HTTP library. The server listens for requests on port 8080 and responds with a simple "Hello, world!" message. The server also has an echo endpoint that echoes back any message that is posted to it.

The code is structured as follows:

* The `WebServer` object is the main entry point for the program. It creates the actor system, the materializer, and the actor that will handle HTTP requests and responses. It also starts the server and logs a message when the server has started.
* The `RequestHandler` class is the actor that will handle HTTP requests and responses. It contains a single method, `receive`, which is called when the actor receives a message. The `receive` method logs the request and then sends a response back to the client.
* The `route` variable defines the routes that the server will handle. In this case, the server has two routes: one that responds with a simple "Hello, world!" message and one that echoes back any message that is posted to it.

Overall, this code is a simple but complete example of how to create a web server in Scala using Akka HTTP.