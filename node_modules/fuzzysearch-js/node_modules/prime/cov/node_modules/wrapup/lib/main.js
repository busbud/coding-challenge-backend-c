// this exports a "masked" version of the WrapUp class.

var WrapUp = require("./wrapup")

module.exports = function(x){
	return new WrapUp(x)
}
