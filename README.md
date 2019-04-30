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
```
$ confetti [required group_name] [optional variant_name]
```
eg.
```
$ confetti aws work
> Setting aws to "work"
> ~/.aws/credentials -> ~/.aws/work.credentials
> Success (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧
```

As you can see from the example output,
this will symlink `~/.aws/credentials` -> `~/.aws/work.credentials`. If the target file is 
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
`~/dotfiles`. If your workflow involves regularly swapping any such files, the small amount of 
initial configuration can be well worth the cost!

One last feature is that you can specify a top level `common` section that will 
be applied to all other groups. For instance:

```
common:
  targets:
      - ~/dir/some.file
  search_paths:
    - path: ~/some/directory
      recursive: false
```

Now, every group we try to use confetti for will also link `~/dir/some.file`, and
will also use `/some/directory` as a search path.

## Installing

### Linux/MacOS

The easiest way to install `confetti` is with [Scarf](https://scarf.sh).

```bash
$ scarf install confetti
```

will be all you need!


### Source

You can install from source with [stack](https://docs.haskellstack.org/en/stable/README/).

### Windows

Confetti does not currently work on Windows, as there is some reliance on POSIX
for dealing with the file system. In theory it should be fairly straight-forward to
get it working on Windows if anyone is up for implementing it. Feel encouraged to submit a PR
or reach out for any related discussion.

## Contributing

Contributions in any form are welcome!

## Future Features

* Config groups should be able to source bash files or sets of environment variables.
