```groovy
import groovy.json.JsonBuilder
import groovy.json.JsonSlurper
import groovyx.net.http.RESTClient
import org.springframework.beans.factory.annotation.Value
import org.springframework.boot.SpringApplication
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.web.bind.annotation.*

@SpringBootApplication
class DemoApplication {
    static void main(String[] args) {
        SpringApplication.run DemoApplication, args
    }
}

@RestController
@RequestMapping('/api')
class ApiController {

    @Value('${app.url}')
    String appUrl

    @PostMapping('/shorten')
    def shorten(@RequestBody Map<String, String> request) {
        def client = new RESTClient(appUrl)
        def response = client.post(path: '/shorten', body: request)
        response.data
    }

    @GetMapping('/{id}')
    def expand(@PathVariable String id) {
        def client = new RESTClient(appUrl)
        def response = client.get(path: '/' + id)
        response.data
    }
}

@RestController
@RequestMapping('/admin')
class AdminController {

    @GetMapping('/health')
    def health() {
        [status: 'OK']
    }

    @GetMapping('/stats')
    def stats() {
        [total: 1000, active: 800, expired: 200]
    }
}
```

This code is a REST API written in Groovy, a dynamic language that runs on the Java Virtual Machine.

The code is divided into three classes:

* `DemoApplication` is the main application class, which bootstraps the Spring Boot application.

* `ApiController` is a REST controller that exposes two endpoints:
    * `/api/shorten` which takes a JSON payload containing a long URL and returns a shortened URL.
    * `/api/{id}` which takes a shortened URL and returns the original long URL.

* `AdminController` is a REST controller that exposes two endpoints:
    * `/admin/health` which returns a simple "OK" status message to check the health of the application.
    * `/admin/stats` which returns some basic statistics about the number of shortened URLs in the system.

The code uses the `groovyx.net.http` library to make HTTP requests to the `/shorten` and `/` endpoints of the application, and the `groovy.json` library to parse and generate JSON payloads.

The code also uses the Spring Boot `@Value` annotation to inject the value of the `app.url` property from the application's configuration into the `ApiController` class.

Overall, this code is a good example of how to use Groovy to write a REST API with Spring Boot.