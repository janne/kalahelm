'use strict';

require('./index.html');
require("./main.css");
var Elm = require('./Main');

Elm.Main.embed(document.getElementById('main'));
