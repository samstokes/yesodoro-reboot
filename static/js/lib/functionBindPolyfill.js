/*
 * PhantomJS doesn't support Function.prototype.bind:
 * https://groups.google.com/d/msg/phantomjs/r0hPOmnCUpc/uxusqsl2LNoJ
 *
 * This polyfill should make our tests work.  It's taken from
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/bind#Compatibility
 *
 * This doesn't get jslinted since it should only be used for testing.
 * (Old browsers might see it, but they're probably horribly broken anyway.)
 */

if (!Function.prototype.bind) {
  console.log("Polyfilling Function.prototype.bind");

  Function.prototype.bind = function (oThis) {
    if (typeof this !== "function") {
      // closest thing possible to the ECMAScript 5 internal IsCallable function
      throw new TypeError("Function.prototype.bind - what is trying to be bound is not callable");
    }

    var aArgs = Array.prototype.slice.call(arguments, 1), 
        fToBind = this, 
        fNOP = function () {},
        fBound = function () {
          return fToBind.apply(this instanceof fNOP && oThis
                                 ? this
                                 : oThis,
                               aArgs.concat(Array.prototype.slice.call(arguments)));
        };

    fNOP.prototype = this.prototype;
    fBound.prototype = new fNOP();

    return fBound;
  };
}
