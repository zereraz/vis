exports.setOnLoad = function(fn) {
  window.onload = fn;
  return function(){};
}

exports.setOnResize = function(sub) {
  window.onresize = function() {
    sub({width: window.innerWidth, height: window.innerHeight})
  };
}

exports.setOnClick = function(el) {
  return function(sub) {
    if(el) {
      el.addEventListener("click", function(e) {
        sub({x: e.clientX, y: e.clientY})
      });
    } else {
      throw new Error ("element for onClick null");
    }
  }
}
