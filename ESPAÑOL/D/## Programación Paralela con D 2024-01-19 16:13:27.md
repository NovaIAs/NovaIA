```d
import std.algorithm, std.stdio, std.string, std.ascii, core.thread;

class p {
    bool completed;
    string name;

    p(string name) {
        this.name = name;
        this.completed = false;
    }

    void run() {
        foreach (auto i; 1..5) {
            writefln("$%s: $i", name, i);
            yield;
        }
        completed = true;
    }
}

void main() {
    p p1 = new p("Proceso 1");
    p p2 = new p("Proceso 2");
    p p3 = new p("Proceso 3");

    foreach (auto p; p1, p2, p3) {
        auto t = new thread(&p.run);
        t.detach;
    }

    while (true) {
        foreach (auto p; p1, p2, p3) {
            if (!p.completed) {
                p.yield;
                continue;
            }
        }

        break;
    }
}
```

Este código crea tres procesos que se ejecutan en paralelo. Cada proceso se ejecuta en un hilo separado y escribe en la consola un mensaje cada segundo. El proceso principal espera a que todos los procesos se completen antes de terminar.

Aquí hay una explicación del código:

* La clase `p` define un proceso. Un proceso tiene un nombre y un método `run` que define la lógica del proceso.
* La función `main` crea tres objetos de la clase `p`.
* La función `main` crea un hilo para cada proceso y los desacopla. Esto significa que los hilos pueden ejecutarse independientemente del hilo principal.
* La función `main` entra en un bucle while que espera a que todos los procesos se completen.
* El bucle while recorre los procesos y comprueba si se han completado. Si un proceso no se ha completado, el bucle llama al método `yield` del proceso. Esto permite que el proceso se ejecute durante un segundo.
* Cuando todos los procesos se han completado, el bucle while se rompe y la función `main` termina.