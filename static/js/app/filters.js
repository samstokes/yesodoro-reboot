'use strict';

angular.module('app.filters', [])


/*
 * Based on Angular's builtin 'filter', but for filtering on object properties.
 * Accepts a property name as a string, and returns objects in the array who
 * have a truthy value for that property.  Works for object methods, static
 * properties, or dynamic getters (__defineGetter__).
 *
 * e.g. <div ng-repeat="obj in objs | filterP:'isAwesome'">
 *      would show all objs for which either obj.isAwesome is truthy, or
 *      obj.isAwesome() returns truthy.
 */
.filter('filterP', function () {
  return function(array, propName) {
    if (!isArray(array)) return array;

    var filtered = [];
    for ( var j = 0; j < array.length; j++) {
      var value = array[j];
      var property = value[propName];
      var propValue;
      if (typeof(property) === 'function') {
        propValue = property.call(value);
      } else {
        propValue = property;
      }
      if (propValue) {
        filtered.push(value);
      }
    }
    return filtered;
  };
})


/*
 * Filter for repeating a set number of times.  e.g.
 *     <div ng-repeat="bottle in 99 | dummyList track by $index">
 *       {{99 - $index}} bottles of beer on the wall...
 */
.filter('dummyList', function () {
  return function (n) {
    return new Array(n);
  };
});
