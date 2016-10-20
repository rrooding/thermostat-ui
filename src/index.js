require('./main.css');

var Elm = require('./Main.elm');

var root  = document.getElementById('root');

Elm.Main.embed(root);

// Disable safari from scrolling the window
document.ontouchmove = function(event){
    event.preventDefault();
}
