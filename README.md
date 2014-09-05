# MOAR dotfiles #

This is just another iteration in the long evolution of my dotfiles repo.  I started with
[dfm](https://github.com/justone/dfm) which worked well for a while, but as usual I overcomplicated
things.  Inspired by the simplicity of people I admire like [sdball](https://github.com/sdball/dotfiles)
and [garybernhardt](https://github.com/garybernhardt/dotfiles), I started over with just a simple
home directory repo.  I am sure it's not perfect and I'll keep iterating, but that's what keeps us
developers happy anyway.

## Components ##

I use the following pieces and parts:

* *homebrew*
* *chruby* -- for managing ruby versions
* *.oh-my-zsh*
* *emacs*
* *vim*
* *tmux* -- WIP

## Basic setup ##

1. ```git init``` in the home directory
1. ```git remote add origin <path/to/repo>```
1. ```git fetch origin```
1. ```git reset --hard origin/master```

### For machine specific changes ###
1. ```git checkout -b <machine_name>```
