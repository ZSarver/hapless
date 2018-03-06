"use strict";

exports.debugBox = function() {
  var d = document.createElement("textarea")
  document.body.append(d)
  return d
}

exports.toDebug = function(text) {
  return function(textBox) {
    return function() {
      textBox.value = text
    }
  }
}

exports.fromDebug = function(textBox) {
  return function() {
    return textBox.value
  }
}
