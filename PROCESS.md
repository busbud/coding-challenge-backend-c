Voici le challenge complété 

Résultat :

https://damp-ravine-39376.herokuapp.com/suggestions/?q=Montreal
https://damp-ravine-39376.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
https://damp-ravine-39376.herokuapp.com/suggestions?latitude=43.70011&longitude=-79.4163 ( pas demandé)


Process de développement
------------------------

01 / Lire le cahier des charges
02 / Ecrire les users stories fonctionnelles ( notamment le scoring)
03 / fork et clone du repository
04/ draft de l'architecture cible
05/ installation de l'environnement (es6/es7, babel, rollup, prettier, etc..)
06/ ajout des librairies via NPM
07/ Création des scripts pour importer les données Geoname
08/ Création des fonctions PostgreSql pour le scoring
09/ faire tourner tout les tests fonctionnelles et vérifier qu'ils sont bien failed
10/ Developper le plus rapidement possible pour avoir un premier résultat (un MVP pour verifier que cela fonctionne)
11: Vérifier si les tests sont au vert.
11/ Je refactore en découpant le code dans les bons fichiers avec unit-test (pas assez par manque de temps)
12/ it works :-)
13/ création du compte heroku
14/ création de PostgreSQL sur Heroku (avec les scripts)
15/ Déployer sur Heroku pour voir si cela fonctionne
16/ pull-request


Base de données
----------------

J'ai installé 2 extensions :

- unaccent pour gérer la recherche avec accent
- postgis pour faire le calcul de distance

Script de base de données
-------------------------

Pour des raisons de simplification et de performance, j'utilise des fonctions sql de PostgreSQL:
- scoring_latlng_name.sql
- scoring_latlng.sql
- scoring_name.sql

Import Data:
------------

J'ai importé le fichier cities_canada-usa.tsv directement avec les commandes import de PostgreSql (commande \copy) mais il faudrait créer des connecteurs (microservice) qui gèrent les mises à jour des données même si théoriquement il y a peu de changement dans le référentiellement.

Scoring :
---------

La difficulté était de gérer 3 types de requêtes :

- requete avec un nom 
https://damp-ravine-39376.herokuapp.com/suggestions/?q=Montreal

- requete avec latitude et longitude 
https://damp-ravine-39376.herokuapp.com/suggestions?latitude=43.70011&longitude=-79.4163

- requete avec un nom, latitude et longitude 
https://damp-ravine-39376.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

J'ai choisis d'utiliser 2 algorithmes differents en fonction de la requête :

- basé sur le nom : j'utilise l'algorithme levenshtein pour mesurer la distance entre les lettres

- basé sur la latitude et la longitude : j'utilise l'algorithme qui calcule la distance entre le point spécifié dans la requete et celle qui se trouve pour chaque ville en base de données. J'applique ensuite un ratio. par exemple lorsque la distance est comprise entre 400km et 500km alors on applique un ratio de 0.7. ( voir seeders/scoring_latlng.sql )

- basé sur le nom, la latitude et la longitude: j'utilise un mix entre les 2 methodes précédentes, j'utilise la formule suivante :

    0.2*algo_nom() + 0.8*algo_latlgn()  --> il y a un poids plus élevé sur la latitude et la longitude car je considère que le client accorde une importance plus grande sur la latitude/longitude que sur le nom.


es6/7 :
-------

S'agissant d'un POC (Proof of Concept), j'ai décidé d'utilisé es6/es7 avec les nouvelles fonctionnalités (async/await, class, etc...) et parce que c'etait fun de le faire :-)
Typescript est aussi une bonne alternative mais on peut pas tout faire :-).

Bundle :
--------

J'ai utilisé rollup pour créer un bundle pour n'avoir qu'un fichier à déployer.


Test d'intégration :
--------------------

Ceux fournit par busbud sont aux verts :-) 

Test unitaire :
---------------

Je n'ai implémenté qu'un test unitaire  ( j'ai honte ) mais c'était pour  montrer que je sais en ecrire. En regle générale, je serais le premier à crier si quelqu'un ne fait pas de tests . Mais le manque de temps m'oblige à faire ce raccourci.

Documentation :
-------------------------

En règle générale, je n'ecris pas de documentation si c'est pour une utilisation interne car le code bouge avec le temps et les developpeurs ne mettent jamais à jour la documentation.
Je milite pour de la "documentation executable" avec le BDD à l'aide de cucumber-js. (https://github.com/cucumber/cucumber-js)


Par contre, si l'API est pour une utilisation externe, alors la documentation est importante.

j'en ai profité pour ajouter un code coverage (Istanbul)

```
npm run unit-test
```

Mitigation:
-----------

J'ai utilisé les streams intensivement pour streamer le flux depuis la base de données jusqu'à la génération du json pour être consommé par le client. Je pense egalement que si le client "abuse", on peut rajouter dans le pipe un "ralentisseur" pour detecter si le nombre de villes dépasse un certain seuil.

