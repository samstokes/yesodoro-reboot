/*exported
   isArray
 , mergeInto
 , getProperty
 */

// borrowed from Angular source
function isArray(value) {
  'use strict';
  return Object.prototype.toString.call(value) === '[object Array]';
}

function mergeInto(dest, src) {
  'use strict';
  for (var k in src) {
    if (src.hasOwnProperty(k))
      dest[k] = src[k];
  }
  return dest;
}

function getProperty(value, propName) {
  'use strict';
  var property = value[propName];
  var propValue;
  if (typeof(property) === 'function') {
    propValue = property.call(value);
  } else {
    propValue = property;
  }
  return propValue;
}
