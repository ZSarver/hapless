"use strict";

exports.debugBox = function() {
  var debug = document.getElementById("debug")
  var d = document.createElement("textarea")
  debug.append(d) 
  return d
}

exports.setDivContents = function(text) {
  return function(divId) {
    return function() {
    var div = document.getElementById(divId)
    div.innerText = text
    }
  }
}

exports.combatLog = function(text) {
    return function() {
      var log = document.getElementById("log")
      log.append("\n")
      log.append(text)
      log.scrollTop = log.scrollHeight
    }
}


exports.fromDebug = function(textBox) {
  return function() {
    return textBox.value
  }
}
