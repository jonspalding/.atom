StepperView = require './stepper-view'
views = require '../util/views'
{span} = views.tags

module.exports =
class Stepper

  constructor: ({@buttons}) ->
    @views = []
    @text = "Grand Steppin'"
    @buttons ?= [
      {icon: 'arrow-down'}
      {icon: 'link-external'}
      {icon: 'chevron-right'}
    ]

  attach: (ed) ->
    @views.push new StepperView ed, @line

  setText: (@text) ->
    for view in @views
      view.clear()
      view.appendChild views.render span 'stepper-label', text
      view.appendChild view.buttonGroup @buttons

  edsForFile: (file) ->
    atom.workspace.getTextEditors()
      .filter((x)->x.getPath() is file)

  activate: (file, line) ->
    active = atom.workspace.getActiveTextEditor()
    if active?.getPath() is file
      active.setCursorBufferPosition [line, 0]
      Promise.resolve()
    else
      atom.workspace.open file,
        initialLine: line
        searchAllPanes: true
        # TODO: This causes Atom to hang when the current stepper-enabled
        # editor is also marked pending.
        # pending: true

  goto: (file, @line) ->
    @listener ?= atom.workspace.observeTextEditors (ed) =>
      @attach(ed) if ed.getPath() is @file

    @activate(file, @line).then =>
      if file == @file
        view.goto @line for view in @views
      else
        @file = file
        @detach()
        @attach(ed) for ed in @edsForFile file
        @setText @text

  detach: ->
    view?.destroy() for view in @views
    @views = []

  destroy: ->
    @detach()
    @listener?.dispose()
    delete @listener
    delete @file
    delete @line
