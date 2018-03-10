"use strict";

exports.debugBox = function() {
  var debug = document.getElementById("debug")
  var d = document.createElement("textarea")
  debug.append(d) 
  return d
}

exports.putCardText = function(cardNumber) {
  return function(text) {
    return function() {
      var s = "card " + cardNumber
      var div = document.getElementById("card " + cardNumber)
      div.innerText = text
    }
  }
}

exports.clearCardText = function(cardNumber) {
  return function() {
    var s = "card " + cardNumber
    var div = document.getElementById("card " + cardNumber)
    div.innerText = ""
  }
}

exports.toDebug = function(text) {
  return function(textBox) {
    return function() {
      textBox.value = text
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
