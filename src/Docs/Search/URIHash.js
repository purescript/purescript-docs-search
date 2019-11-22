/* global exports history */

// https://stackoverflow.com/questions/1397329
function removeHash () {
    var scrollV, scrollH, loc = window.location;
    if ("pushState" in history)
        history.pushState("", document.title, loc.pathname + loc.search);
    else {
        scrollV = document.body.scrollTop;
        scrollH = document.body.scrollLeft;

        loc.hash = "";

        document.body.scrollTop = scrollV;
        document.body.scrollLeft = scrollH;
    }
}

exports.getInput = function () {
  var hash = document.location.hash;
  if (hash.slice(0, 8) == "#search:") {
    return decodeURIComponent(hash.slice(8));
  } else {
    return "";
  }
};

exports.setInput = function (input) {
  return function () {
    if (!input) {
      removeHash();
    } else {
      document.location.hash = "search:" + encodeURIComponent(input);
    }
  };
};

exports.clearInput = removeHash;
