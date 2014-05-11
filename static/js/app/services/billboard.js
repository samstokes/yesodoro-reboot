/*global
   angular
 */

angular.module('app.services')
.factory('Billboard', function ($rootScope) {
  'use strict';

  var REQUIRED_EVENT_PROPERTIES = [
    'message',
    'severity'
  ];

  var scope = $rootScope.$new();

  var Billboard = {};

  Billboard.watch = function watch(watchFn) {
    scope.$watch('lastEvent', watchFn);
  };

  function validateEvent(event) {
    function invalid(reason) {
      throw new Error('Invalid event: ' + reason);
    }

    REQUIRED_EVENT_PROPERTIES.forEach(function (property) {
      if (!event[property]) invalid('must have "' + property + '" property');
    });
  }

  Billboard.notify = function notify(severity, message) {
    var event = {severity: severity, message: message};
    validateEvent(event);
    scope.lastEvent = event;
  };

  Billboard.clear = function clear() {
    scope.lastEvent = null;
  };

  ['success', 'error'].forEach(function (severity) {
    Billboard[severity] = Billboard.notify.bind(Billboard, severity);
  });

  return Billboard;
});
