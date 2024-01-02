# TP1 : Gobblet

Dans ce premier travail pratique, vous devez réaliser l'implémentation en Haskell d'un jeu
de plateau nommé Gobblets.

Le travail doit se faire individuellement.

Le travail est à remettre pour le **31 octobre 2023** sur l'espace de rendu situé sur le
Moodle du cours. Ce sujet a été publié le 19 septembre 2023.


## Historique des modifications apportées à ce sujet

+ 2023-09-20 (vers midi). Corrections dans les fichiers `gobblet.cabal` et `Test.hs`.
+ 2023-09-26. Ajout d'un exemple concernant l'élément `onboard` de la grammaire de
  mouvements.
+ 2023-10-16. Dans la partie 2, `stack run test-minimax` a été corrigé en
  `stack run test-score`.


## Objectifs

Les objectifs de ce travail sont les suivants :

+ se familiariser avec le paradigme fonctionnel au travers du langage Haskell ;

+ exploiter la simplicité d'un langage fonctionnel ;

+ se confronter à l'implantation d'éléments concrets dans un langage fonctionnel pur ;

+ comprendre les patrons de conception et d'architecture fonctionnels ;

+ exploiter le typage fort pour guider sa solution.


## Éléments d'évaluations

Le travail pratique sera évalué en fonction des critères suivants.

| # | Élément                                      | Points   |
|---|----------------------------------------------|----------|
| 1 | Implantation des règles                      | 20       |
| 2 | Implantation du score                        | 20       |
| 3 | Interface utilisateur                        | 15       |
| 4 | Documentation existante et claire            | 15       |
| 5 | Clarté du code                               | 15       |
| 6 | Utilisation adaptée du paradigme fonctionnel | 15       |
| 7 | Élément(s) au choix                          | +20      |

+ Chacun de ces éléments est décrit plus bas dans ce document.

+ Les projets sont donc notés sur 100, avec en plus 20 points bonus qui peuvent être obtenus
  avec l'élément 7.


## Projet initial

+ Ce dépôt GitLab sert de projet initial.

+ Vous devez vous en servir comme base pour votre TP.

+ Vous pouvez en faire un _fork_ sur le GitLab du département.

+ Vous pouvez également utiliser la commande suivante pour le récupérer localement :

```sh
git clone https://gitlab.info.uqam.ca/inf6120/232-TP1
```

+ Complétez immédiatement le fichier `README.md` avec votre nom et code permanent.


## 1. Règles

Les règles du jeu *Gobblet* sont disponibles
[ici](https://www.jeuxavolonte.asso.fr/regles/gobblet.pdf), ou
[ici](https://www.youtube.com/watch?v=YVxVMa-XSLE) sous forme de vidéo.

La bonne implantation des règles sera évaluée avec une suite de tests. Le projet initial
vous fournit une première suite de tests pour évaluer votre implantation.

Pour tester vos règles, lancez la commande

``` sh
stack run test-rules
```

Vous pouvez inspecter la liste des mouvements disponibles de référence en regardant les
fichiers `.moves` présent dans le dossier `tests/`. Chaque test contient la liste des
mouvements attendus pour le plateau qu'il représente.


## 2. Score

Il est demandé d'implanter la fonction de score pour un plateau de jeu, qui correspond à la
définition suivante :

+ Le score du plateau est égal à `Score1 - Score0` où `Score1` est le score du joueur
  courant, et `Score0` est le score de son adversaire, sur ce plateau.

+ Le score d'un joueur est égal à `Alignement2 + 10 * Alignement3` où `Alignement2` est le
  nombre d'alignements potentiels de longueur 2 sur le plateau, et `Alignement3` est le
  nombre d'alignements potentiels de longueur 3 sur le plateau.

+ Chaque alignement n'est compté qu'une seule fois : un alignement de 3 ne compte que comme
  un alignement de 3, et non pas comme deux alignement de 2 supplémentaires.

+ Les alignements ne sont considérés que sur les lignes, colonnes et diagonales où il serait
  possible d'aligner 4 pièces.

+ Un alignement _potentiel_ est un alignement qui peut être partiellement recouvert par des
  pièces adverses. C'est-à-dire qu'une pièce du joueur `X` recouverte par une pièce du
  joueur `O` peut compter dans un alignement pour `X`.

+ On ignore dans ce score les alignements de 4 pièces.

Par exemple, sans désigner la taille des pièces et sans avoir de pièces recouvertes par une
autre, le plateau suivant aurait une valeur de 10 pour le joueur `O`, et de 1 pour le joueur
`X`, donc un score de 9, car c'est au tour de `O`.

```
O X X _
X O _ _
_ _ O _
_ _ _ _
```

Voici le détail du calcul:

+ Joueur `X` a un alignement de deux avec les pièces placées en (1, 0) et (2, 0), ce qui
  fait un score de 1 (l'origine du système de coordonnées est en haut à gauche).

+ Joueur `X` n'a pas d'alignement de deux avec les pièces placées en (1, 0) et (0, 1), car
  cette paire de pièce n'est pas placée sur une diagonale où il serait possible de faire un
  alignement de 4.

+ Joueur `O` a un alignement de trois avec les pièces placées en (0, 0), (1, 1), et (2, 2).
  Cet alignement compte donc pour 10 points.

+ On a donc 1 point pour `X` et 10 pour `O`. Le score du plateau, où c'est au tour de `O` de
  jouer, et de `10 - 1 = 9`.


L'implantation de la fonction de score se fait dans la fonction nommée `score`.

Les tests se lancent par
```sh
stack run test-score
```

Cette fonction va nous servir pour créer un adversaire automatique de la manière très
simpliste suivante. Pour jouer son coup, il va considérer tous les coups possibles et il va
choisir celui qui le mène à une situation la plus favorable pour lui, c'est-à-dire celle qui
donne le plus grand score possible.


## 3. Interface utilisateur interactive

Il faut que vous développiez une interface utilisateur dans le terminal afin de pouvoir
affronter l'adversaire automatique décrit dans la partie précédente. L'interface utilisateur
doit se lancer avec la commande

```sh
stack run
```

Vous être libre de choisir la façon dont vous implantez cette interface et d'utiliser une
bibliothèque pour vous aider.

Voici un exemple d'interaction avec une interface possible, où les entrées utilisateurs sont
préfixées par un `>` :

```
Computer is playing drop(B, (0,0))
O3 __ __ __ 
__ __ __ __ 
__ __ __ __ 
__ __ __ __ 

O2O1O0 O3O2O1O0 O3O2O1O0  || X3X2X1X0 X3X2X1X0 X3X2X1X0 
> drop(B, (1, 1))
Playing: drop(B, (1,1))
Computer is playing drop(M, (0,1))
O3 __ __ __ 
O2 X3 __ __ 
__ __ __ __ 
__ __ __ __ 

O1O0 O3O2O1O0 O3O2O1O0  || X2X1X0 X3X2X1X0 X3X2X1X0 
```


## 4. Documentation

Votre projet doit être documenté en utilisant le format Haddock. La documentation doit
pouvoir être générée par la commande

``` sh
stack exec -- haddock --html src/*.hs --hyperlinked-source --odir=docs/
```

La documentation se retrouve dans le dossier `docs/`. Vous pouvez écrire votre documentation
en français ou en anglais. Cette documentation servira également à évaluer la bonne
utilisation du paradigme fonctionnel dans votre projet.


## 5. Clarté du code

Votre code doit être clair et concis. Vous pouvez vous référer aux [Programming
Guidelines](https://wiki.haskell.org/Programming_guidelines) du HaskellWiki, ainsi
qu'utiliser l'outil [hslint](https://github.com/ndmitchell/hlint) pour vous aider.


## 6. Utilisation adaptée du paradigme fonctionnel

Cet élément sera évalué sur base de votre documentation et votre code.


## 7. Élément au choix

Vous pouvez implanter une extension au projet de base. Si cela est correctement réalisé,
cela pourra vous donner des points supplémentaires.

Vous pouvez choisir l'extension que vous souhaitez. Voici quelques exemples de possibilités
d'extension :

+ architecture client/serveur ;

+ interface graphique ;

+ heuristique de minimax complémentaire pour améliorer le niveau de jeu de l'adversaire
  automatique ;

+ recherche de meilleur mouvement par la méthode de Monte-Carlo et des simulations de
  parties ;

+ sauvegarde et chargement de partie, ainsi que des configurations du jeu via un fichier de
  configuration.

Si vous souhaitez implanter une autre extension, il est recommandé d'en parler avec votre
professeur.

Dans tous les cas, **décrivez l'élément implantté dans votre fichier `README.md`**, sans
quoi il ne sera pas pris en compte durant la notation.


## Suites de tests

Le dossier `tests` contient une suite de tests servant à valider votre implantation. Chaque
fichier dans ce dossier représente une partie en cours ou terminée, ainsi que le nombre de
mouvements possibles au prochain tour de la partie, et le score du plateau pour la
partie.


### Format d'un test

Un fichier test est structuré de la manière suivante :

+ la première ligne est un nombre, indiquant de nombre de mouvements possibles pour la
  partie (pour l'état du jeu après avoir effectué tous les mouvements) ;

+ la seconde ligne est un nombre, indiquant le score du plateau pour la partie (pour l'état
  du jeu après avoir effectué tous les mouvements) ;

+ Le reste du fichier représente une partie sous forme d'une suite de mouvements.

La lecture et le lancement des tests est déjà implanté, excepté la lecture des mouvements.
Il vous faudra donc implanter la fonction

```haskell
parseMoves :: [String] -> [Move]
```

dans `Move.hs` pour pouvoir lancer les tests. Il est conseillé d'utiliser la bibliothèque
[parsec](https://hackage.haskell.org/package/parsec) pour lire mouvements. Cette libraire,
bien documentée, est aussi décrite dans des livres, tel
qu'[ici](https://book.realworldhaskell.org/read/using-parsec.html).


### Grammaire des mouvements

Les mouvements sont représentés selon la grammaire

```
size ::= T | S | M | B
piece ::= size
x, y ::= 0 | 1 | 2 | 3
position ::= (x, y)
move ::= drop(piece, position) | onboard(position, position)
```

La taille des pièce est ordonnée de la façon suivante : `T < S < M < B` (pour _tiny_,
_small_, _medium_, et _big_). Par exemple, le mouvement "_Joueur X place une grande pièce de
sa réserve en (0, 1)_" est représenté par

```
drop(B, (0, 1))
```

et le mouvement "_Joueur X déplace la pièce visible en (0, 2) vers (2, 1)_" est représenté
par

```
onboard((0, 2), (2, 1))
```

Les espaces sont optionnels.

### Grammaire d'une partie

Une partie est représentée par un mouvement sur chaque ligne. La première ligne représente
le premier mouvement effectué par le premier joueur. Chaque ligne qui suit représente un
mouvement effectué à partir du plateau courant.


## Remise

La remise se fait sur Moodle. Créez une archive `zip` ou `tar.xz` de votre dossier du TP et
transférez là sur le devoir « TP1 » disponible sur la page du cours. Vous pouvez par exemple
récupérer une archive directement depuis votre dépôt GitLab ou utiliser la commande

```sh
TP=$(basename $(pwd))
cd ..
tar cJf $TP.tar.xz $TP
```

depuis la racine de votre répertoire du TP. Envoyez ensuite le fichier `.tar.xz` ainsi créé.

Votre travail doit être remis au plus tard le **31 octobre 2023 à 23:59**. À partir de
minuit, une pénalité de **2 points par heure entamée** sera appliqué, avec un retard maximal
de **24 heures**. Un travail remis à 00:00 ou 00:59 aura donc une pénalité de 2 points,
tandis qu'un travail remis à 01:00 aura une pénalité de 4 points.

**Aucune remise par courriel** ne sera acceptée. Une remise **au delà de 24 heures**
après la date limite **ne sera pas acceptée.**

## Checklist

Vérifiez bien les éléments suivants lors de votre remise :

+ [ ] le fichier `README.md` contient votre nom et votre code permanent ;

+ [ ] Le fichier `README.md` décrit l'extension implantée, le cas échéant ;

+ [ ] le code compile avec `stack build` ;

+ [ ] le code est documenté et la documentation peut être générée avec la commande indiquée
  plus haut ;

+ [ ] les tests de règle passent ;

+ [ ] les tests de score passent ;

+ [ ] l'interface graphique fonctionne ;

+ [ ] Le code est compressé et envoyé sur la page Moodle du cours.

