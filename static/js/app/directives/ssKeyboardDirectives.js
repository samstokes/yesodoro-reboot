/*global angular */

angular.module('app.directives')
.directive('ssEscape', function ($parse) {
  'use strict';

  return function (scope, element, attrs) {
    var fn = $parse(attrs.ssEscape);
    element.bind('keydown', function (event) {
      if (event.keyCode === 27) {
        scope.$apply(function () {
          fn(scope, {$event: event});
        });
      }
    });
  };
})
.directive('ssCtrlEnter', function ($parse) {
  'use strict';

  return function (scope, element, attrs) {
    var fn = $parse(attrs.ssCtrlEnter);
    element.bind('keydown', function (event) {
      if ((event.keyCode === 10 || event.keyCode === 13) && event.ctrlKey) {
        scope.$apply(function () {
          fn(scope, {$event: event});
        });
      }
    });
  };
});
