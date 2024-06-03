**Nom du fichier : systeme_expert.scm**

**Description :** Ce code implémente un système expert simple en SCHEME. Il permet de diagnostiquer des pannes informatiques en posant des questions à l'utilisateur.

**Code :**

```scheme
(define (systeme-expert)
  (let ((base-de-connaissances
          '(
            ((symptome 1) (cause 1))
            ((symptome 2) (cause 2))
            ((symptome 3) (cause 3))
            ((symptome 4) (cause 4))
          ))
        (questions
          '(
            "Quel est le symptôme 1 ?"
            "Quel est le symptôme 2 ?"
            "Quel est le symptôme 3 ?"
            "Quel est le symptôme 4 ?"
          )))
    (lambda (utilisateur)
      (let loop ((i (length questions)) (causes '()))
        (cond
          ((= i 0) (car (assoc (last causes) base-de-connaissances)))
          (else
            (display (list-ref questions (- i 1)))
            (newline)
            (set! causes (cons (read-string) causes))
            (loop (- i 1) causes)))))))

(define expert (systeme-expert))
```

**Explication du code :**

* La **base de connaissances** est une liste de paires (symptôme, cause).
* La liste **questions** contient les questions à poser à l'utilisateur.
* La fonction **systeme-expert** retourne une fonction qui prend un utilisateur en argument.
* La fonction retournée utilise une boucle itérative pour parcourir la liste de questions.
* À chaque itération, elle affiche une question et lit la réponse de l'utilisateur.
* Elle met à jour une liste de causes possibles en fonction des réponses de l'utilisateur.
* Lorsque toutes les questions ont été posées, elle renvoie la cause la plus probable.

**Exemple d'utilisation :**

```scheme
(define user (lambda (prompt) (print prompt) (read-string)))

(displayln "Bienvenue dans le système expert.")
(displayln "Je vais vous poser une série de questions pour diagnostiquer votre panne informatique.")
(newline)

(displayln "Résultat : " (expert user))
```