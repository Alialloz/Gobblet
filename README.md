# Gobblet


## Identification

- Auteur(s)  : `NAYERI POOR Ali` 

## Extensions Réalisées

Le jeu implémente les fonctionnalités suivantes :

- **Algorithme MiniMax**: L'IA du jeu utilise l'algorithme MiniMax pour calculer les meilleurs mouvements.
- **Niveaux de difficulté de l'IA**: Trois niveaux de difficulté sont disponibles pour l'IA, allant d'aléatoire à des profondeurs de recherche plus avancées dans l'algorithme MiniMax.
- **Modes de jeu**: Jouez contre un autre joueur humain ou affrontez l'IA.
- **Classes de jeu**: Le projet intègre des classes pour gérer les états du jeu (`State`), les mouvements (`Move`), les règles (`Rules`), et le score (`Score`).

## À propos du projet

Le projet `Gobblet` est une implémentation du jeu de société classique en Haskell, une langue de programmation fonctionnelle. Le but de ce projet est de fournir une plateforme interactive pour jouer à Gobblet contre une IA ou un autre joueur humain. L'IA peut fonctionner à différents niveaux de difficulté grâce à l'algorithme MiniMax avec des profondeurs de recherche variables.

## Installation

Pour commencer à utiliser `Gobblet`, suivez les étapes ci-dessous :

1. Clonez le dépôt sur votre machine locale en utilisant `git clone` suivi de l'URL de ce dépôt.
2. Assurez-vous que vous avez [GHC](https://www.haskell.org/ghc/) et [Stack](https://docs.haskellstack.org/en/stable/README/) installés sur votre ordinateur.
3. Naviguez dans le dossier du projet et exécutez `stack build` pour compiler le projet.
4. Lancez le jeu en exécutant `stack exec gobblet`.

## Utilisation

Après avoir lancé le jeu, suivez les instructions à l'écran pour choisir entre jouer contre un autre joueur ou contre l'IA. Si vous jouez contre l'IA, vous aurez également la possibilité de choisir le niveau de difficulté.

Vous pouvez faire des mouvements en entrant des commandes textuelles, telles que `drop(S, (0, 0))` pour déposer un pion, ou `move((0, 0), (1, 0))` pour déplacer un pion.

Le jeu et son interface graphique est utilisable via le terminal. L'éxcécution de 'stack run' permet de lancer le jeu. 

Si jamais l'utilisateur veut jouer contre une IA il aura la possibilité de choisir entre 3 niveaux de difficultées : 
        - Facile : IA joue aléatoirement ses coups 
        - Modéré : IA adoptant un algorithme minimax à profondeur 3
        - Difficile : IA adoptant un algorithme minimax à profondeur 4

Une fois ces choix effectués, la partie peut commencer ! 

## Règles du Jeu

Gobblet se joue sur un plateau carré de 4x4 cases. Chaque joueur dispose de 12 pièces de différentes tailles, appelées gobelets, réparties en 3 piles. Les joueurs peuvent poser un nouveau gobelet sur le plateau, déplacer un gobelet déjà joué sur une case vide, ou recouvrir un gobelet plus petit avec un plus grand, que ce soit le leur ou celui de leur adversaire.

## But du Jeu

Le but de Gobblet est de créer une rangée de quatre gobelets de sa couleur. Cette rangée peut être horizontale, verticale ou diagonale. La stratégie et la mémoire jouent un rôle crucial dans ce jeu, car vous devez vous souvenir des tailles des gobelets sous les piles et anticiper les mouvements de votre adversaire.

## Déroulement Classique d'une Partie

1. **Initialisation** : Le plateau est vide. Les joueurs placent leurs piles de gobelets à côté du plateau.
2. **Début de la Partie** : Le joueur avec les gobelets X commence la partie.
3. **Tour de Jeu** :
    - **Placer** : Le joueur peut choisir de placer un gobelet du sommet d'une de ses piles sur n'importe quelle case vide du plateau ou une case adverse completant un alignement de 3.
    - **Déplacer** : Alternativement, le joueur peut déplacer un gobelet déjà sur le plateau vers une contenant un gobelet plus petit que lui ou une case vide.
    - **Recouvrir** : Un joueur peut également recouvrir un gobelet plus petit avec un gobelet plus grand, peu importe la couleur du gobelet inférieur.
4. **Fin de la Partie** : La partie se termine quand un joueur aligne quatre de ses gobelets en ligne, en colonne ou en diagonale. Si aucun joueur ne peut faire un mouvement valide, la partie se termine par une égalité.


## Contribution

Si vous souhaitez contribuer au projet, veuillez me contacter sur mon email étudiant : nayeri_poor.ali@courrier.uqam.ca.

