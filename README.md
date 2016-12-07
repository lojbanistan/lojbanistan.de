# lojbanistan.de

`lojbanistan.de` sammelt deutsche Materialien zu der Sprache [Lojban](https://mw.lojban.org/papri/Lojban).


# autoren & beteiligte
[@Profpatsch](http://profpatsch.de)  
[@uwap](https://github.com/uwap)


# tech

Basiert auf [Html5Boilerplate](https://html5boilerplate.com/).
Nutzt [Hakyll](https://jaspervdj.be/hakyll/).
Greift auf die Daten von [jbovlaste](http://jbovlaste.lojban.org/) zurück.

# dev

    cabal run watch
    
Mit [nix](http://nixos.org/nix/)-Unterstützung:

    cabal2nix . > lojbanistan-de.nix
    nix-shell
    cabal run watch
    
Dies ist das einzige Projekt, bei dem die Entwicklungssprache ausschließlich in Deutsch und Lojban gehalten wird.

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.
