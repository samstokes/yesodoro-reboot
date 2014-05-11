/*global angular */

angular.module('app.directives')
.directive('billboard', function (Billboard) {
  'use strict';

  return {
    restrict: 'AE',

    scope: {},

    replace: true,
    template: '\
        <div ng-show="event" class="{{event.severity}}"> \
          <span class="message">{{event.message}}</span> \
          <a class="dismiss" ng-click="dismiss()">X</a> \
        </div>\
        ',

    controller: function ($scope) {
      $scope.dismiss = Billboard.clear.bind(Billboard);

      Billboard.watch(function billboardDirectiveWatch(event) {
        if (event) {
          $scope.event = event;
        } else {
          $scope.event = null;
        }
      });
    }
  };
});
