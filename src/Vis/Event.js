exports.setOnLoad = function(fn) {
  window.onload = fn;
  return function(){};
}

exports.setOnResize = function(sub) {
  window.onresize = function() {
    sub({x: window.innerWidth, y: window.innerHeight})
  };
  return function(){};
}

exports.setOnClick = function(el) {
  return function(sub) {
    if(el) {
      var cb = function(e) {
        sub({x: e.clientX, y: e.clientY})
      }
      el.addEventListener("click", cb);
      return function(){};
    } else {
      throw new Error ("element for onClick null");
    }
  }
}
