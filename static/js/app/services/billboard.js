/*global
   angular
 */

angular.module('app.services')
.factory('Billboard', function ($rootScope) {
  'use strict';

  var scope = $rootScope.$new();

  var Billboard = {};

  Billboard.watch = function watch(watchFn) {
    scope.$watch('lastEvent', watchFn);
  };

  Billboard.notify = function notify(severity, message) {
    scope.lastEvent = {severity: severity, message: message};
  };

  Billboard.clear = function clear() {
    scope.lastEvent = null;
  };

  ['success', 'error'].forEach(function (severity) {
    Billboard[severity] = Billboard.notify.bind(Billboard, severity);
  });

  return Billboard;
});
