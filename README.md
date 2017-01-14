# Industrial strength irc bot for more coucou

With support for TLS !

# Dev

## Build
`stack build` et c'est partit


## Modules utiles

  * `Run.hs` le fichier principal.
  * `Command.hs` Le traitement des commandes.
  * `Types.hs` tous les types ainsi que les instances JSON.
  * `Parser.hs` le parseur pour aller de `message` vers `CoucouCmd`.
  * `Factoids.hs` les factoids.
  * `Social.hs` info liées à un nick particulier, décompte de coucou etc.
  * `Cancer.hs` récupère, parse et formatte le cancer du web. En partenariat avec [lelele.io](lelele.io)

# License
BSD3 (see LICENSE file)
