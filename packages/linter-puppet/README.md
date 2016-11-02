# linter-puppet

This is a linter package to lint puppet files using the puppet parser validate command.

## Installation

* Install puppet
* `$ apm install linter` (if you don't have already).
* `$ apm install linter-puppet`

## Settings

(Optional) You can configure the settings by editing ~/.atom/config.cson:

    'linter-puppet':
      'puppetExecutablePath': null # puppet path. run 'which puppet' to find the path
