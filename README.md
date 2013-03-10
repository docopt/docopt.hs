# Docopt.hs

### A Haskell port of python's [docopt](http://docopt.org).

TODO: 

- + Split the monolithic Docopt.hs into private submodules
- ± Test the public API
	- + fix [options] parsing of any defined option
	- sort out a way to differentiate boolean flags from options with expected values
	- parse consecutive flags/options
	- parse stacked short flags/options (ie "-aBc")
- ± Sort out the .cabal config
- Write a helpful README