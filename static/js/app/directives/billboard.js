/*global angular */

angular.module('app.directives')
.directive('billboard', function (Billboard) {
  'use strict';

  return {
    restrict: 'AE',

    scope: {},

    replace: true,
    template: '\
        <div ng-show="message" class="{{severity}}"> \
          <span class="message">{{message}}</span> \
          <a class="dismiss" ng-click="dismiss()">X</a> \
        </div>\
        ',

    controller: function ($scope) {
      $scope.dismiss = Billboard.clear.bind(Billboard);

      Billboard.watch(function billboardDirectiveWatch(event) {
        if (event) {
          $scope.message = event.message;
          $scope.severity = event.severity;
        } else {
          $scope.message = null;
        }
      });
    }
  };
});
