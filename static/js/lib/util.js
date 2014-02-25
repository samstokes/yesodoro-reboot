'use strict';

// borrowed from Angular source
function isArray(value) {
  return toString.call(value) === '[object Array]';
}

function mergeInto(dest, src) {
  for (var k in src) {
    if (src.hasOwnProperty(k))
      dest[k] = src[k];
  }
  return dest;
}
