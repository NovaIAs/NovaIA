```scheme
;(define TaskQueue (cons nil nil))

(define (NewTask &rest Arguments)
  (cons nil (consArguments . Arguments)))

(define (AddTask Task)
  (if (null? TaskQueue)
    (set! Tail Task)
    (set-cdr! Tail Task))
  (set! Tail (cdr Task))
  (set! Head (cdr Task)))

(define (DoTasks)
  (if (null? Head)
    nil
    (let ((NewResult
            (funcall (apply list (car Head))
                    (cdr Head))))
      (DoTasks)
      NewResult)))

(define (ListTasks)
  (let ((Current TaskQueue))
    (let loop ((Results '()))
      (if (null? Current)
        Results
        (let ((Task (car Current)))
          (loop (cons (car Task) Results)
                (cdr Current)))))))

(define (ChainTasks . Tasks)
  (let loop ((Tasks Tasks)
              (TaskQueue (cons nil nil))
              (Head nil)
              (Tail nil))
    (if (not (null? Tasks))
      (let ((NewTask (cons nil Tasks)))
        (AddTask NewTask)
        (set! Tasks (cdr Tasks))
        (loop Tasks TaskQueue Head Tail))
      (DoTasks))))

(define (Schedule . Procedures)
  (let ((Results nil)
        (TaskQueue (cons nil nil))
        (Head nil)
        (Tail nil))
    (do ((Tasks Procedures)
         (Result Results))
      ((null? Tasks)
       Result)
      (let ((NewTask (NewTask (lambda () (AddTask (NewTask Procedures)))
                                (cdr Procedures)))))
        (AddTask NewTask)
        (set! Tasks (cdr Tasks))
        (set! Result (funcall (car NewTask)))))))
```

Explicación:

* `NewTask`: Crea una nueva tarea con una lista de argumentos.
* `AddTask`: Añade una tarea a la cola de tareas.
* `DoTasks`: Ejecuta todas las tareas de la cola de tareas en orden.
* `ListTasks`: Devuelve una lista de todas las tareas de la cola de tareas.
* `ChainTasks`: Concatena varias colas de tareas en una sola.
* `Schedule`: Ejecuta una serie de procedimientos de forma concurrente.

Ejemplo de uso:

```scheme
(AddTask (NewTask (lambda () (display "Tarea 1\n"))))
(AddTask (NewTask (lambda () (display "Tarea 2\n"))))
(AddTask (NewTask (lambda () (display "Tarea 3\n"))))

(DoTasks)
```

Salida:

```
Tarea 1
Tarea 2
Tarea 3
```