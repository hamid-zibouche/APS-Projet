# README.md

## Installation

### Prérequis
Avant de commencer, assurez-vous d'avoir les outils suivants installés sur votre machine :

- **Make** : utilisé pour la gestion de la compilation
- **SWI-Prolog** : utilisé pour le typeur (`typeur.pl`)

### Installation des dépendances

1. **Cloner le dépôt** :
   
   ```bash
   git clone https://github.com/hamid-zibouche/APS-Projet.git

   cd APS-Projet
   ```

2. **Installer SWI-Prolog** (si ce n'est pas déjà fait) :
   
   Pour Ubuntu :
   
   ```sh
   sudo apt-get install swi-prolog
   ``` 

3. **Vérifier que `make` et `swipl` sont installés** :
   
   ```sh
   make --version
   swipl --version
   ``` 

4. **Assurez-vous que `ocamlyacc` est installé dans `/usr/bin/`**  
   Cette condition est indispensable pour compiler les fichiers correctement, que ce soit via la commande `make` ou à l'aide des scripts shell (`.sh`).

### Lancement des Tests avec typeurOnly.sh et evaluateurOnly.sh

Chaque répertoire de langage contient :

- **typeurOnly.sh** : Ce script lance l'ensemble des tests pour le **typeur**. Il parcourt tous les fichiers `.aps` dans le répertoire `Test/` (situé à la racine du projet) et exécute pour chacun la commande suivante :
  
  ``` bash
  ./prologTerm <fichier_test.aps> | swipl typeur.pl
  ```

  Pour lancer les tests du dossier Test pour le typeur, placez-vous dans le répertoire concerné (par exemple, `APS0`) puis exécutez :

  ``` bash
  ./typeurOnly.sh
  ```

- **evaluateurOnly.sh** : Ce script lance l'ensemble des tests pour l'**évaluateur**. Chaque test est exécuté avec un timeout (par défaut 3 secondes) pour éviter les boucles infinies. La commande pour un test est :

  ``` bash
  timeout 3s ./evaluateur <fichier_test.aps>
  ```

  Pour lancer ces tests du dossier Test pour l'évaluateur, placez-vous dans le répertoire concerné et exécutez :

  ``` bash
  ./evaluateurOnly.sh
  ```

### Script Global all.sh

- Choix interactif du langage (APS0, APS1, APS1a, APS2)
- Compilation du code dans le répertoire choisi (`make`)
- Lancement des tests (du typeur ou de l'évaluateur) sur l'ensemble des fichiers présents dans le répertoire `Test/`
- Nettoyage global des répertoires de langage avec `make clean` lors de la fermeture

Vous pouvez lancer le script global avec :

```sh
./all.sh
```

Ce script interactif vous permettra de faire vos tests en chaîne sans devoir relancer manuellement les différentes commandes.
