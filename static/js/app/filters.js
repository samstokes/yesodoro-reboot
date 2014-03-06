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
  return function filterPFilter(array, propName) {
    if (!isArray(array)) return array;

    var filtered = [];
    for ( var j = 0; j < array.length; j++) {
      var value = array[j];
      var propValue = getProperty(value, propName);
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
  return function dummyListFilter(n) {
    var array = new Array(n);
    // ng-repeat complains if the list complains duplicates, as a plain
    // "new Array(n)" would (since undefined === undefined).  So generate some
    // gratuitous diversity.
    for (var i = 0; i < n; ++i) array[i] = i;
    return array;
  };
})


/*
 * Group the input objects by a named field: e.g.
 * <div ng-repeat="group in people | groupBy:'homeTown'">
 *   <h2>{{group.homeTown}}</h2>
 *   <p ng-repeat="person in group.items">{{person.name}}</p>
 *
 * N.B. in practice this can't actually be used with ng-repeat, as the group
 * objects it produces are different (by object identity) each time, so the
 * digest cycle never converges.  "track by" should be able to solve this, but
 * didn't seem to help.  So instead, use it in a controller (via $filter) to
 * pre-group your data.  You'll need a $watchCollection to update the groupings
 * when the data changes, and anything which edits the objects *in* the list
 * (e.g. result of XHR calls) will need to update the groupings too.
 *
 * TODO maybe we can solve this by clever memoisation?
 */
.filter('groupBy', function () {
  function Group(initialItem) {
    this.items = [initialItem];
  }

  return function groupByFilter(items, groupField) {
    var groups = {}, filtered = [];
    angular.forEach(items, function (item) {
      var groupValue = getProperty(item, groupField);
      var group;
      if (groups.hasOwnProperty(groupValue)) group = groups[groupValue];
      if (group === undefined) {
        group = new Group(item);
        group[groupField] = groupValue;
        groups[groupValue] = group;
        filtered.push(group);
      } else {
        group.items.push(item);
      }
    });
    return filtered;
  };
});
