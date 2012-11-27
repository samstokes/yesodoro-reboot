Util = {
  merge: function merge (dest, src) {
    for (var k in src) {
      if (src.hasOwnProperty(k)) {
        dest[k] = src[k];
      }
    }
    return dest;
  }
};
