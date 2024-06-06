**Gestion automatisée des serveurs avec Ansible**

```shell
#!/bin/bash

# Variables de configuration
ansible_hostfile=/etc/ansible/hosts
ansible_playbook=/etc/ansible/playbooks/serveurs.yml
ansible_user=ansible

# Vérification des permissions
if [ $(id -u) -ne 0 ]; then
    echo "Vous devez être root pour exécuter ce script" >&2
    exit 1
fi

# Vérification des fichiers de configuration
if [ ! -f $ansible_hostfile ]; then
    echo "Fichier d'hôtes manquant : $ansible_hostfile" >&2
    exit 1
fi
if [ ! -f $ansible_playbook ]; then
    echo "Playbook manquant : $ansible_playbook" >&2
    exit 1
fi

# Exécution d'Ansible
ansible-playbook -i $ansible_hostfile $ansible_playbook -u $ansible_user
```

**Explication du code :**

Ce script shell gère l'automatisation des tâches d'administration de serveurs à l'aide d'Ansible. Voici une explication étape par étape du code :

1. **Variables de configuration** : Ce sont les paramètres utilisés par Ansible, tels que le fichier d'hôtes, le playbook et l'utilisateur.

2. **Vérification des permissions** : Le script vérifie si l'utilisateur exécutant le script est root. Seul root peut exécuter des commandes sudo et gérer des serveurs.

3. **Vérification des fichiers de configuration** : Le script vérifie si les fichiers Ansible nécessaires (fichier d'hôtes et playbook) existent et sont accessibles.

4. **Exécution d'Ansible** : La commande `ansible-playbook` est utilisée pour exécuter le playbook Ansible, qui définit les tâches à effectuer sur les serveurs distants. Les options `-i` (fichier d'hôtes), `-u` (utilisateur) et `-vvvv` (niveau de verbosité) sont utilisées pour configurer l'exécution.

Ce script permet d'automatiser les tâches d'administration du serveur, telles que la mise à jour des logiciels, la configuration des services et le déploiement d'applications. Il simplifie la gestion de plusieurs serveurs et réduit les erreurs humaines.