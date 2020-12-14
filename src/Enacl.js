"use static";

var blake2b = require('blake2b');

exports.generichashImpl = function(i) {
    return function(b) {
	      return blake2b(i).update(b).digest().buffer;
    };
};
