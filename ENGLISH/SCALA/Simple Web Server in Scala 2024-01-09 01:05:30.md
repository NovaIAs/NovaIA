```scala
// This is a Scala program that implements a simple web server.

// Import the necessary libraries.
import akka.actor.{ Actor, ActorSystem, Props }
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpResponse, HttpRequest, StatusCodes }
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.util.Timeout

// Define the actor that will handle HTTP requests.
class RequestHandler extends Actor {
  def receive = {
    case HttpRequest(GET, Uri.Path("/"), _, _, _) =>
      sender ! HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Hello, world!</h1>"))
    case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
      sender ! HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "pong"))
    case HttpRequest(GET, Uri.Path("/crash"), _, _, _) =>
      throw new RuntimeException("Simulated crash!")
    case _ =>
      sender ! HttpResponse(StatusCodes.NotFound, entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "Not found"))
  }
}

// Create an actor system and a request handler actor.
val system = ActorSystem("webserver")
val requestHandler = system.actorOf(Props[RequestHandler], "request-handler")

// Define the route that will be used to handle HTTP requests.
val route: Route =
  path("/") {
    get {
      complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Hello, world!</h1>")))
    }
  } ~
  path("/ping") {
    get {
      complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "pong")))
    }
  } ~
  path("/crash") {
    get {
      complete(HttpResponse(StatusCodes.InternalServerError, entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "Simulated crash!")))
    }
  }

// Bind the route to the HTTP server.
Http().bindAndHandle(route, "0.0.0.0", 8080)

// Await the termination of the actor system.
system.whenTerminated

// This is the explanation of the code:

// The first lines of the code import the necessary libraries.

// The `RequestHandler` class defines the actor that will handle HTTP requests.
// The actor receives HTTP requests and responds to them.

// The `system` variable creates an actor system.
// The actor system is a container for actors.

// The `requestHandler` variable creates a request handler actor.
// The request handler actor is responsible for handling HTTP requests.

// The `route` variable defines the route that will be used to handle HTTP requests.
// The route maps HTTP requests to responses.

// The `Http()` method binds the route to the HTTP server.
// The HTTP server listens on port 8080.

// The `system.whenTerminated` method awaits the termination of the actor system.
// This is necessary to prevent the program from exiting before the actor system has terminated.
```