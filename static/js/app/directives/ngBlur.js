/*global angular */

// this may be redundant in newer versions of Angular
angular.module('app.directives')
.directive('ngBlur', function ($parse) {
  'use strict';

  return function (scope, element, attrs) {
    var fn = $parse(attrs.ngBlur);
    element.bind('blur', function (event) {
      scope.$apply(function () {
        fn(scope, {$event: event});
      });
    });
  };
});
