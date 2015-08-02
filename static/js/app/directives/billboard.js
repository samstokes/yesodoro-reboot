/*global angular */

angular.module('app.directives')
.directive('billboard', function ($window, Popup, Billboard) {
  'use strict';

  return {
    restrict: 'AE',

    scope: {},

    replace: true,
    template: '\
        <div ng-class="{empty: !event}" class="billboard {{event.severity}}"> \
          <div ng-show="event.message"> \
            <span class="message">{{event.message}}</span> \
            <a class="dismiss" ng-click="dismiss(event)">X</a> \
          </div> \
          <div ng-show="event.action"> \
            <a class="action" ng-click="performAction(event.action)">{{event.action.message}}</a> \
          </div> \
        </div> \
        ',

    link: function (scope, elem, attrs) {
      scope.dismiss = Billboard.clear.bind(Billboard);

      var POPUP_OPTIONS = {
        height: 300,
        width: 400,
        location: true
      };

      function reload() {
        $window.location.reload();
      }

      scope.performAction = function performAction(action) {
        if (action.reload !== undefined) {
          reload();
        } else if (action.url) {
          var popup = Popup.open(action.url, POPUP_OPTIONS);

          if (action.onCloseMessage) {
            popup.then(function onPopupClose() {
              Billboard.success(action.onCloseMessage, {timeout: 10000});
            });
          }
        }

        this.dismiss(scope.event);
      };

      Billboard.watch(function billboardDirectiveWatch(event) {
        if (event) {
          scope.event = event;
        } else {
          scope.event = null;
        }
      });
    }
  };
});
