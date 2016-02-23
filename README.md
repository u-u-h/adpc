This is a fork of the Algebraic Dynamic Programming compiler code from
<http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html>. The
original code seems to not be maintained anymore; it was
Copyright (C) 2001-2008 Peter Steffen, Marco Ruether, Christian Lang,
Georg Sauthoff, Stefanie Schirmer.

The original authors are currently not involved in this github fork of
their code, so please do not contact them with problems, unless you
are sure that their original code is concerned.

On the other hand, I'd be happy if any of the original contributors
would be interested in further work on this.

Until we have autoconf infrastructure, the original build and install
instructions found in [INSTALL](INSTALL) apply. 
Basically, you'll need a working Haskell installation. For now I'm 
aiming at GHC 7.10 or later. Backward compatibility is probably lost along
the way.

* For OSx: 

	```
	$ brew install Caskroom/cask/haskell-platform
	```

* For Debian-based Linuxes: 

	```
	$ apt-get install haskell-platform
	```

and also install the haxml package: 

```
$ cabal update  
$ cabal install haxml
```
	
Be sure to have the directory where cabal installs executables in you path, e.g.
`~/Library/Haskell/bin/` on OSx.
	
Look at the original [README](README) file for usage instructions. The
code is free under [GPL V2](COPYING) license.

uuh/2016-02-23
