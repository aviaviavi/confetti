Confetti
==========

[![Build Status](https://travis-ci.org/aviaviavi/confetti.svg?branch=master)](https://travis-ci.org/aviaviavi/confetti)

Confetti is a tiny tool for managing different versions of a particular
configuration file, and quickly swapping them.<br />

Confetti is set up via a spec file, `~/.confetti.yml`. 
You can specify one or more `groups`, each with one or more `targets`, 
which are the config files you wish to manage. 

Suppose we have multiple AWS credential files we want to easily switch between (ie use various `~/.aws/credentials`
variants). To use confetti for this task, we'll put our different files in the target directory, `~/.aws`, 
named `~/.aws/${variant-name}.credentials`. We might have:

```
~/.aws/personal.credentials
~/.aws/work.credentials
~/.aws/org_name.credentials
```

A simple example group specification in `~/confetti.yml` might look like:
```
groups:
  - name: aws
    targets:
      - /home/you/.aws/credentials
```

To switch to `work.credentials` simply run: 
```$ confetti [required group_name] [optional variant_name]```
eg.
```$ confetti aws work```

This will symlink `~/.aws/credentials` -> `~/.aws/work.credentials`. If the target file is 
_not_ a symlink when you invoke confetti, a backup will be made before your variant file is 
linked. If you have multiple target files in your group, they will all be symlinked to their
respective variants.

You can specify alternative `search_paths` for each `group`, and whether or not to search recursively.
Adding a new group with this functionality could make our .confetti.yml look like:

```
groups:
  - name: aws
    targets:
      - ~/.aws/credentials
  - name: dotfiles
    targets:
      - ~/.spacemacs
      - ~/.vimrc
      - ~/.zshrc
      - ~/.zpreztorc
      - ~/.tmux.conf
      - ~/.tmux.style.conf
    search_paths:
      - path: ~/dotfiles
        recursive: true
```

This makes confetti a great tool for managing things like dotfiles, where the target config files
are located in a github repo anywhere on the machine. Simply running:
`confetti dotfiles` could link `~/.spacemacs` -> the first `.spacemacs` found in a recursive search of
`~/dotfiles`. This results in a potential alternative to something like GNU Stow, provided that your use case
is swapping out particular sets of files often.


## Installing

### MacOS

You can install with brew via:
```
$ brew tap aviaviavi/homebrew-tap
$ brew install aviaviavi/homebrew-tap/confetti
```

### Linux 

A linux binary can be found on the releases page.

### Source (Cross Platform)

You can install from source with [stack](https://docs.haskellstack.org/en/stable/README/).

## Contributing

Contributions in any form are welcome!

## Future Features

* Config groups should be able to source bash files or sets of environment variables.
