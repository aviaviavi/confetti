Confetti
==========

[![Build Status](https://travis-ci.org/aviaviavi/confetti.svg?branch=master)](https://travis-ci.org/aviaviavi/confetti)

Confetti is a tiny tool for managing different versions of a particular
configuration file, and quickly swapping them.<br />

Confetti is set up via a spec file, `~/.confetti.yml`. 
You can specify one or more `groups`, each with one or more `targets`, 
which are the config files you wish to manage. 

Suppose we have multiple AWS credential files we want to easily switch between (ie use various `~/.aws/credentials`
variants).
To use confetti for this task, we'll put our different files in the target directory, `~/.aws`, 
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

To switch to `work.credientials` simply run: 
```$ confetti $group_name $variant_name```
eg.
```$ confetti aws work```

This will symlink `~/.aws/credentials` -> `~/.aws/work.credentials`<br />

If the target file is _not_ a symlink when you invoke confetti, a backup will be made 
before your variant file is linked.

If you have multiple target files in your group, they will all be symlinked to the variant.


