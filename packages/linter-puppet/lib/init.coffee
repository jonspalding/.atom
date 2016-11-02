module.exports =
  config:
    puppetExecutablePath:
      type: 'string'
      default: ''
      title: 'Puppet Executable Path'

  activate: ->
    console.log 'activate linter-puppet'
