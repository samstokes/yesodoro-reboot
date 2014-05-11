/*global
   angular
 */

angular.module('app.services')
.factory('Billboard', function ($rootScope, $timeout) {
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

    if (event.action) validateAction(event.action);
  }

  function validateAction(action) {
    function invalid(reason) {
      throw new Error('Invalid event action: ' + reason);
    }

    if ((action.reload !== undefined) === (!!action.url)) { // jshint ignore:line
      invalid('must have either "reload" or "url" properties but not both');
    }

    if (action.reload !== undefined && !action.reload) invalid('"reload: ' + action.reload + '" does not make sense');

    if (action.url && !isRelativeUrl(action.url)) {
      invalid("can't popup absolute URLs");
    }

    if (action.onCloseMessage && !action.url) {
      invalid('"onCloseMessage" can only be used with a url');
    }
  }

  function isRelativeUrl(url) {
    return url[0] === '/' && url[1] !== '/';
  }

  Billboard.notify = function notify(severity, message, properties) {
    var timeout, action;
    if (properties) {
      timeout = properties.timeout;
      action = properties.action;
    }

    var event = {severity: severity, message: message};
    if (action) {
      event.action = action;

      if (!event.action.message) {
        event.action.message = 'Click here';
      }
    }
    validateEvent(event);
    scope.lastEvent = event;

    if (timeout > 0) {
      $timeout(function notifyTimeout() {
        this.clear(event);
      }.bind(this), timeout);
    }

    return event;
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
