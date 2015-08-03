beforeEach(function () {
  jasmine.addMatchers({
    toBeEmpty: function () {
      function dumpArrayLike(array) {
        if (typeof(array) === 'string') return '"' + array + '"';

        if (typeof(array.join) === 'function') {
          return '[' + array.join(', ') + ']';
        }

        var arrayified = Array.prototype.slice.call(array);
        return dumpArrayLike(arrayified);
      }

      return {
        compare: function toBeEmpty(actual) {
          var result = {
            pass: actual.length === 0
          };
          if (result.pass) {
            result.message = 'Expected ' + dumpArrayLike(actual) + ' not to be empty';
          } else {
            result.message = 'Expected ' + dumpArrayLike(actual) + ' to be empty';
          }
          return result;
        }
      };
    },

    toBeAPromise: function () {
      return {
        compare: function toBeAPromise(actual) {
          var result = {
            pass: actual && typeof(actual.then) === 'function'
          };
          return result;
        }
      };
    }
  });
});
