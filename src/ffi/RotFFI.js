"use strict";


exports._init = function(options) {
  return function () {
    var display = new ROT.Display(options);
    document.body.appendChild(display.getContainer());
    return display
  }
}

exports._initKeyboardHandler = function() {
    var handler = { handle: function(e) { } }
    document.addEventListener('keydown', function(event) { handler.handle(event) })
    return handler
}

exports._getKey = function (handler) {
  return function (onError, onSuccess) {
    handler.handle = onSuccess
    return function (cancelError, cancelerError, cancelerSuccess) {
      req.cancel()
      handler.handle = function() {}
      cancelerSuccess()
    }
  }
}

exports._clear = function(display) {
  return function() {
    display.clear()
  }
}



