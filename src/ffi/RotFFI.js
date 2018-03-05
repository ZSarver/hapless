"use strict";


exports._initrotjs = function() {
  var h = 0
  return function(options) {
    return function () {
      if (h) {
        console.error("Cannot init multiple instances of rotjs")
        return h
      } else {
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

        h = {}
        h.display = new ROT.Display(o);
        h.keyCallback = function(e) { }

        document.addEventListener('keydown', function(event) { h.keyCallback(event) })
        document.body.appendChild(h.display.getContainer());
        return h
      }}}}()

exports._ready = function(onError, onSuccess) {
  window.setTimeout(onSuccess, 250)
  return function (cancelError, cancelerError, cancelerSuccess) {
    cancelerSuccess()
  }
}

exports._putTile = function(tile) {
  return function(x) {
    return function(y) {
      return function(h) {
        return function() {
          h.display.draw(x,y,tile)
        }}}}}

exports._putTile2 = function(fg) {
  return function(bg) {
    return function(x) {
      return function(y) {
        return function(h) {
          return function() {
            h.display.draw(x,y,[bg, fg])
          }}}}}}

exports._getKey = function (h) {
  return function (onError, onSuccess) {
    h.keyCallback = onSuccess
    return function (cancelError, cancelerError, cancelerSuccess) {
      req.cancel()
      h.keyCallback = function() {}
      cancelerSuccess()
    }
  }
}

exports._clear = function(h) {
  return function() {
    h.display.clear()
  }
}



