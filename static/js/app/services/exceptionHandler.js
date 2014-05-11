/*global
   angular
 */

angular.module('app.services')
.config(function ($provide) {
  'use strict';

  $provide.decorator('$exceptionHandler', function ($delegate, $injector, $log) {
    var Billboard;
    return function billboardExceptionHandler(exception, cause) {
      /*
       * Have to inject Billboard at runtime to late-resolve circular
       * dependency between Billboard and $exceptionHandler.
       */
      Billboard = Billboard || $injector.get('Billboard');

      Billboard.error(exception.name + ': ' + exception.message, {timeout: 10000});
      $delegate(exception, cause);
    };
  });
});
