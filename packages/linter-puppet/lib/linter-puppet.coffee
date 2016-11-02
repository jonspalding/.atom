linterPath = atom.packages.getLoadedPackage("linter").path
Linter = require "#{linterPath}/lib/linter"
{CompositeDisposable} = require 'atom'

class LinterPuppet extends Linter
  # The syntax that the linter handles. May be a string or
  # list/tuple of strings. Names should be all lowercase.
  @syntax: ['source.puppet']

  # A string, list, tuple or callable that returns a string, list or tuple,
  # containing the command line (with arguments) used to lint.
  cmd: 'puppet parser validate'

  executablePath: null

  linterName: 'puppet'

  errorStream: 'stderr'

  # A regex pattern used to extract information from the executable's output.
  # e.g. Error: Could not parse for environment production: Syntax error at '{'; expected '}' at /home/user/init.pp:11
  regex: '.*: (?<message>(Syntax|Unclosed|Could) (.|\\n)*) at (.|\\n)*:(?<line>\\d+)'

  constructor: (editor) ->
    @disposables = new CompositeDisposable
    super(editor)

    @disposables.add atom.config.observe 'linter-puppet.puppetExecutablePath', =>
      @executablePath = atom.config.get 'linter-puppet.puppetExecutablePath'

  destroy: ->
      super
      @disposables.dispose()

  createMessage: (match) ->
    # message might be empty, we have to supply a value
    if match and match.type == 'parse' and not match.message
      message = 'parse error'
    super(match)

module.exports = LinterPuppet
