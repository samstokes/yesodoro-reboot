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
          <div> \
            <span class="message">{{event.message}}</span> \
            <a class="dismiss" ng-click="dismiss(event)">X</a> \
          </div> \
          <div ng-show="event.action"> \
            <a ng-click="performAction(event.action)">{{event.action.message}}</a> \
          </div> \
        </div> \
        ',

    controller: function ($scope, $window) {
      $scope.dismiss = Billboard.clear.bind(Billboard);

      var POPUP_FEATURES = [
        'height=300',
        'width=400',
        'menubar=0',
        'toolbar=0',
        'location=1'
      ].join(',');

      function popup(url) {
        return $window.open(url, 'popup', POPUP_FEATURES);
      }
      function reload() {
        $window.location.reload();
      }

      $scope.performAction = function performAction(action) {
        if (action.reload !== undefined) {
          reload();
        } else if (action.url) {
          var poppedUp = popup(action.url);
          if (action.onCloseMessage) {
            poppedUp.onunload = function onPopupClose() {
              Billboard.success(action.onCloseMessage, {timeout: 10000});
            };
          }
        }

        this.dismiss();
      };

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