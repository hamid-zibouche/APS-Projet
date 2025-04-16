# README.md

## 1. Avancement du projet

Nous avons réalisé les étapes suivantes du projet : **APS0, APS1, APS1a, APS2 et APS3**.  
Toutes ces étapes fonctionnent correctement, et nous les avons validées à l’aide de nombreux tests, aussi bien pour la **partie typage** que pour la **partie sémantique**. À notre connaissance, les fonctionnalités attendues à chaque étape sont opérationnelles. Etant deux pour ce projet, nous avons la majorité du temps travaillé ensemble soit lors des séances de TME soit en distanciel via Discord pour les ajustements de code.

## 2. Fonctionnalités

### 2.1 Fonctionnalités fonctionnelles

Voici la liste des fonctionnalités implémentées et fonctionnelles à ce jour :

- **APS0** :
  - Déclaration de constantes (`CONST`) de type `bool` et `int`
  - Définition de fonctions (`FUN`) et de fonctions récursives (`FUN REC`) ayant pour corps une expression
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
  - Ajout des fonctions procédurales et des fonctions récursives procédurales qui utilisent obligatoirement la commande `RETURN` à la fin de leur bloc pour renvoyer un résultat 

### 2.2 Fonctionnalités non fonctionnelles

À notre connaissance, toutes les fonctionnalités listées ci-dessus fonctionnent correctement, avec des tests concluants pour chacune. Aucun bug ou comportement inattendu n’a été relevé pour le moment.

## 3. Choix d'implémentation

### 3.1 Gestion des primitives
La gestion des primitives est effectuée par l’utilisation d’un `match` sur l’`ASTApp` afin de traiter chaque cas individuellement. Il nous suffit à chaque fois de repérer quelle opération est appelée par son nom et nous y effectuons l'opération associée. Par exemple, l'addition se traduit par un `ASTId("add")` lors de l'évaluation de l'application: nous effectuons donc directement l'addition des arguments listés. Il en est de même pour toutes les autres primitives. 

### 3.2 Gestion de la mémoire, de l'environnement et du flux de sortie
Nous avons opté pour une implémentation utilisant des listes de couples. Ces listes sont manipulées avec la fonction `List.assoc` pour la récupération et la gestion des valeurs associées. Pour avoir un espace d'adressage correct, c'est à dire des adresses différentes à chaque fois que nous en avons besoin, nous utilisons une `variable globale` en tant que compteur qu'il nous suffit d'incrémenter à chaque création de nouvelles adresses. La mémoire est donc une liste de couples `(adresse,valeur)`, l'environnement une liste de `(chaine de caractères,valeur)` et le flux de sortie une liste de valeurs (qui sont des entiers).

### 3.3 Gestion de l'environnement de typage
Le typage a été fait intégralement en Prolog comme imposé. Pour les types de base (`true`, `false`, types des primitives), nous les avons implémentés en dur dans un environnement que nous initialisons obligatoirement à chaque tentative de typage. Ensuite, nous avons des règles de typage spécifiques pour chaque expression, instruction, définition et suite de commandes créées en suivant exactement la spécification de base.

### 3.3 Précision au niveau de APS3
Comme écrit dans le cours, les arguments des nouvelles fonctions sont les arguments de base des anciennes fonctions d'APS0. Nous n'y avons pas ajouté le traitement des adresses de variables en tant qu'arguments et il n'est donc par conséquent pas possible de modifier la valeur d'un argument dans le corps de la fonction. Les fonctions doivent obligatoirement renvoyer un résultat via un RETURN (dans le cas contraire, le typage échoue) à l'inverse des procédures.

## 4. Extensions

Nous avons suivi la spécification initiale et avons simplement ajouté la possibilité d'insérer des commentaires dans le code APS en utilisant la syntaxe `(* commentaire *)`. Il nous a pour cela fallu modifier le parser afin d'ignorer toute chaine de caractères entre les deux délimiteurs. Les commentaires ne sont utilisables qu'en `APS3`.

## 5. Tests

Pour chaque APS, le premier réflexe de test a toujours été de nous assurer que les fichiers de test de l'APS précédent avaient toujours le même comportment dans le nouvel APS (à part pour `APS1a`). Voici quelques cas limites que nous avons en plus jugé importants de tester:

- le typage et la sémantique des abstractions en tant qu'arguments d'appel d'une fonction en APS0 (Test/t_09.aps)
- la vérification du changement des valeurs aussi bien pour les constantes que pour les variables dans les procédures de APS1 (Test/t_03.aps et Test/t_04.aps)
- la restriction du changement de valeurs via SET aux variables et le traitement des adresses en arguments de procédures en APS1a (Test/t_04.aps)
- la création et le traitement de matrices en APS2 (Test/t_03.aps)
- la détection par le typeur de la présence de code mort dans les fonctions en APS3 (code après un RETURN)
- la possibilité d'avoir des commandes IF et WHILE qui renvoient un résultat ou ne renvoient rien en fonction des cas en APS3 (Test/t_11_APS3.aps)



