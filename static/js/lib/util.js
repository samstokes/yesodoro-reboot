/*exported
   isArray
 , mergeInto
 , getProperty
 , zeroPad
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

// ridiculous function to zero pad numbers as strings
// only works for integers < 100
function zeroPad(num) {
  'use strict';

  return num < 10 ? '0' + num : num;
}
