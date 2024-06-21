**Programme de recherche et de filtrage d'enregistrements**

Dans ce programme, nous allons créer une liste d'enregistrements contenant des informations sur les étudiants. Nous fournirons ensuite un menu d'options pour rechercher et filtrer ces enregistrements en fonction de critères spécifiques.

**Code :**

```portgol
# Déclaration des variables
lista_registros = []
continuer = True

# Fonction de création d'enregistrement
def criar_registro(nome, matricula, curso):
    registro = {
        "nome": nome,
        "matricula": matricula,
        "curso": curso
    }
    return registro

# Fonction d'affichage d'un enregistrement
def exibir_registro(registro):
    print(f"Nome: {registro['nome']}")
    print(f"Matrícula: {registro['matricula']}")
    print(f"Curso: {registro['curso']}")
    print("------------------------------")

# Fonction de recherche d'un enregistrement par nom
def pesquisar_por_nome(lista, nome):
    for registro in lista:
        if registro["nome"] == nome:
            return registro
    return None

# Fonction de filtragem des enregistrements par cours
def filtrar_por_curso(lista, curso):
    registros_filtrados = []
    for registro in lista:
        if registro["curso"] == curso:
            registros_filtrados.append(registro)
    return registros_filtrados

# Boucle principale
while continuar:
    # Affichage du menu d'options
    print("Menu :")
    print("[1] Criar um novo registro")
    print("[2] Pesquisar um registro por nome")
    print("[3] Filtrar registros por curso")
    print("[4] Sair")

    # Récupération du choix de l'utilisateur
    escolha = int(input("Digite sua escolha: "))

    # Traitement du choix de l'utilisateur
    if escolha == 1:
        # Création d'un nouveau registre
        nome = input("Digite o nome do aluno: ")
        matricula = input("Digite a matrícula do aluno: ")
        curso = input("Digite o curso do aluno: ")
        novo_registro = criar_registro(nome, matricula, curso)
        lista_registros.append(novo_registro)
        print("Registro criado com sucesso.")

    elif escolha == 2:
        # Recherche d'un enregistrement par nom
        nome = input("Digite o nome do aluno a ser pesquisado: ")
        registro_encontrado = pesquisar_por_nome(lista_registros, nome)
        if registro_encontrado:
            exibir_registro(registro_encontrado)
        else:
            print("Registro não encontrado.")

    elif escolha == 3:
        # Filtrage des enregistrements par cours
        curso = input("Digite o curso a ser filtrado: ")
        registros_filtrados = filtrar_por_curso(lista_registros, curso)
        if registros_filtrados:
            print("Registros filtrados:")
            for registro in registros_filtrados:
                exibir_registro(registro)
        else:
            print("Nenhum registro encontrado para o curso especificado.")

    elif escolha == 4:
        # Sortie du programme
        continuar = False
        print("Saindo do programa.")

    else:
        # Choix invalide
        print("Escolha inválida. Tente novamente.")
```

**Explication du code :**

* **Variables :**
    * `lista_registros` : Liste contenant les enregistrements des étudiants.
    * `continuer` : Booléen indiquant si le programme doit continuer à s'exécuter.

* **Fonctions :**
    * `criar_registro` : Crée un nouveau registre avec les informations fournies.
    * `exibir_registro` : Affiche les informations d'un registre donné.
    * `pesquisar_por_nome` : Recherche un enregistrement dans la liste en fonction du nom fourni.
    * `filtrar_por_curso` : Filtre les enregistrements dans la liste en fonction du cours fourni.

* **Boucle principale :**
    * Affiche un menu d'options à l'utilisateur.
    * Récupère le choix de l'utilisateur et exécute l'action correspondante.
    * Quitte le programme lorsque l'utilisateur choisit de sortir.

Ce programme permet aux utilisateurs de créer, rechercher et filtrer des enregistrements d'étudiants en fonction de critères spécifiques, démontrant ainsi les capacités avancées de PORTUGOL.