## 1. Avancement du projet

Nous avons réalisé les étapes suivantes du projet : **APS0, APS1, APS1a, APS2 et APS3**.  
Toutes ces étapes fonctionnent correctement, et nous les avons validées à l’aide de nombreux tests, aussi bien pour la **partie typage** que pour la **partie sémantique**. À notre connaissance, les fonctionnalités attendues à chaque étape sont opérationnelles.

## 2. Fonctionnalités

### 2.1 Fonctionnalités fonctionnelles

Voici la liste des fonctionnalités implémentées et fonctionnelles à ce jour :

- **APS0** :
  - Déclaration de constantes (`CONST`) de type `bool` et `int`
  - Définition de fonctions (`FUN`) et de fonctions récursives (`FUN REC`)
  - Gestion des primitives (opérations arithmétiques, booléennes, etc.)
  - Instruction `ECHO`
  - Typage et sémantique fonctionnels pour tous les éléments ci-dessus

- **APS1** :
  - Déclaration de variables (`VAR`) et affectation via `SET`
  - Définition de procédures (`PROC`) et procédures récursives (`PROC REC`)
  - Structures de contrôle `IF` et `WHILE` dans les blocs des procédures
  - Appels de procédures via `CALL`

- **APS1a** :
  - Ajout des arguments passés par adresse (`ARGSP`) pour les procédures
  - Gestion des variables passées par adresse

- **APS2** :
  - Gestion des tableaux : `ALLOC`, `NTH`, `LEN`
  - Modification d’un tableau avec `VSET` et `SET`

- **APS3** :
  - Ajout des fonctions procédurales avec la possibilité d’utiliser des `RETURN`

### 2.2 Fonctionnalités non fonctionnelles

À notre connaissance, toutes les fonctionnalités listées ci-dessus fonctionnent correctement, avec des tests concluants pour chacune. Aucun bug ou comportement inattendu n’a été relevé pour le moment.

## 3. Choix d'implémentation

### 3.1 Gestion des primitives
La gestion des primitives est effectuée par l’utilisation d’un `match` sur l’`ASTId` afin de traiter chaque cas individuellement.

### 3.2 Gestion de la mémoire, de l'environnement et du flux de sortie
Nous avons opté pour une implémentation utilisant des listes de couples. Ces listes sont manipulées avec la fonction `List.assoc` pour la récupération et la gestion des valeurs associées.


## 4. Extensions

Nous avons suivi la spécification initiale et avons simplement ajouté la possibilité d'insérer des commentaires dans le code APS en utilisant la syntaxe `(* commentaire *)`.

## 5. Tests

Pour chaque APS, nous avons élaboré des exemples de code spécifiques afin de tester minutieusement chaque fonctionnalité ajoutée.

## 6. APS3 – Fonctions procédurales

Pour APS3, nous avons conçu les fonctions procédurales de sorte qu'elles puissent retourner une valeur. Ces fonctions se composent d'une suite de commandes qui exécutent les traitements désirés.

