"use strict";


exports.init = function(options) {
  console.log(options)
  var display = new ROT.Display(options);
  document.body.appendChild(display.getContainer());
  return function () {
    return display
  }
}

exports.clear = function(display) {
  display.clear()
}



