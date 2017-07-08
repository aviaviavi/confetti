Confetti
==========

[![Build Status](https://travis-ci.org/aviaviavi/confetti.svg?branch=master)](https://travis-ci.org/aviaviavi/confetti)

Confetti is a tiny tool for managing different versions of a particular
configuration file, and quickly swapping them.<br />

```
$ ls ~/.aws

credentials (the credential file we're using at any given time)
work.credentials (credential file for work)
my.credentials (credentials for my personal aws account)
```

With a small amount of yaml based configuration, we could swap out credentials like so:

`$ confetti aws work`

And credentials would be a symlink to work.credentials.

You can also link multiple different config targets together to have groups of config files that can all be swapped with a single command. 

Confetti will read it's configuration from `~/.confetti.yml`. You can even manage your `.confetti.yml` files using `confetti`!
