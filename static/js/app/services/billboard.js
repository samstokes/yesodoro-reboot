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

  var SEVERITIES = [
    'success',
    'error'
  ];

  var scope = $rootScope.$new();

  var Billboard = {SEVERITIES: SEVERITIES};

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

    if (SEVERITIES.indexOf(event.severity) < 0) invalid('unknown severity "' + event.severity + '"');
  }

  Billboard.notify = function notify(severity, message) {
    var event = {severity: severity, message: message};
    validateEvent(event);
    scope.lastEvent = event;
  };

  Billboard.clear = function clear(event) {
    if (event && scope.lastEvent !== event) {
      return;
    }
    scope.lastEvent = null;
  };

  SEVERITIES.forEach(function (severity) {
    Billboard[severity] = Billboard.notify.bind(Billboard, severity);
  });

  return Billboard;
});
