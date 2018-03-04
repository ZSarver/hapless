"use strict";


exports._init = function(options) {
  return function () {
    var o = Object.assign({}, options)
    o.layout ="tile"
    o.tileWidth = o.tileSize
    o.tileHeight = o.tileSize
    var tileSet = document.createElement("img")
    tileSet.src = o.tileSet
    o.tileSet = tileSet
    Object.keys(o.tileMap).map( function(key) {
      o.tileMap[key] = [o.tileMap[key].x * o.tileSize, o.tileMap[key].y * o.tileSize]
    })

    console.log(o)
    var display = new ROT.Display(o);

    document.body.appendChild(display.getContainer());
    return display
  }
}

exports._putTile = function(tile) {
  return function(x) {
    return function(y) {
      return function(display) {
        return function() {
          display.draw(x,y,tile)
        }}}}}

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



