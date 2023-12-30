Claro! Aqui está um exemplo de um código complexo em D que utiliza o padrão Observer para implementar um sistema de notificação em tempo real:

```d
import std.stdio;
import std.conv;
import std.container;

interface Observer {
    void update(string message);
}

class Subject {
    private NotifyList!Observer observers;

    void attach(Observer observer) {
        observers.insert(observer);
    }

    void detach(Observer observer) {
        observers.remove(observer);
    }

    void notify(string message) {
        foreach (observer; observers) {
            observer.update(message);
        }
    }
}

class EmailNotifier : Observer {
    void update(string message) {
        writeln("Enviando email de notificação: ", message);
    }
}

class SmsNotifier : Observer {
    void update(string message) {
        writeln("Enviando SMS de notificação: ", message);
    }
}

void main() {
    Subject subject = new Subject();

    EmailNotifier emailNotifier = new EmailNotifier();
    SmsNotifier smsNotifier = new SmsNotifier();

    subject.attach(emailNotifier);
    subject.attach(smsNotifier);

    subject.notify("Novo pedido recebido!");

    subject.detach(smsNotifier);

    subject.notify("Pagamento confirmado!");

    // Resultado esperado:
    // Enviando email de notificação: Novo pedido recebido!
    // Enviando email de notificação: Pagamento confirmado!
}
```

Neste código, temos a definição de uma interface `Observer` que possui o método `update` para atualizar os observadores. A classe `Subject` é responsável por gerenciar os observadores e notificar quando ocorrerem mudanças. As classes `EmailNotifier` e `SmsNotifier` implementam a interface `Observer` e definem a forma como as notificações devem ser enviadas, neste caso, por email e SMS.

No `main`, criamos uma instância do `Subject` e dos observadores. Em seguida, anexamos os observadores ao `Subject` utilizando o método `attach`. Chamamos o método `notify` para enviar uma notificação de "Novo pedido recebido!" para todos os observadores. Em seguida, removemos o observador de SMS utilizando o método `detach` e chamamos novamente o `notify` para enviar uma notificação de "Pagamento confirmado!" apenas para o observador de email.

Espero que esse exemplo atenda às suas expectativas!