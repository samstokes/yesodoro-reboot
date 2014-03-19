/*global angular */

angular.module('app.directives')
.directive('ssEditablePopup', function () {
  'use strict';

  return {
    restrict: 'EA',

    scope: {
      show: '&ssShow'
    },

    transclude: true,
    replace: true,

    template: ' \
      <div ng-show="show() || inputsFocused &gt; 0" ng-transclude /> \
    ',

    link: function (scope, element, attrs) {
      var inputs = element.find('input, textarea');

      scope.inputsFocused = 0;

      inputs.bind('focus', function (event) {
        scope.$apply(function () { scope.inputsFocused += 1; });
      }).bind('blur', function (event) {
        scope.$apply(function () { scope.inputsFocused -= 1; });
      }).bind('keydown', function (event) {
        if (event.keyCode === 27) {
          this.blur();
        }
      });
    }
  };
});
