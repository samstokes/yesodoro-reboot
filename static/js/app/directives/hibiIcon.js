/*global angular */

angular.module('app.directives')
.directive('hibiIcon', function () {
  'use strict';

  /*
   * If the img tag is contained in these elements, the title tag won't be
   * displayed as a tooltip.
   */
  var TOOLTIP_PARENTS = ['A', 'BUTTON'];

  return {
    restrict: 'AE',
    scope: {
      src: '@',
      title: '@'
    },
    template: '<img alt="{{title}}" />',
    link: function (scope, elem, attrs) {
      var img = elem.find('img');
      scope.$watch('src', function (src) {
        img.attr('src', src);
      });

      var parent = elem.parent();
      if (TOOLTIP_PARENTS.indexOf(parent.prop('tagName')) >= 0) {
        parent.attr('title', scope.title);
      }
    }
  };
});
