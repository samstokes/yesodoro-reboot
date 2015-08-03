/*global
   angular
 */

angular.module('app.services')
.constant('httpErrorHandler', function httpErrorHandler($q, Billboard) {
  'use strict';

  return {
    responseError: function httpErrorHandler(rejection) {
      switch (rejection.status) {
        case 400:
          Billboard.error('Oh dear, something went wrong!', {
            action: {
              reload: true,
              message: 'Click here to try refreshing the page.'
            }
          });
          break;
        case 401:
          Billboard.error('Sorry, your session has expired.', {
            action: {
              message: 'Click here to log in.',
              login: true,
              successMessage: 'Now please try again.'
            }
          });
          break;
        case 500:
          Billboard.error('Sorry, something went wrong!');
          break;
        case 0:
          Billboard.error('Oops, either your wifi or our servers are down!  Please try again later.');
          break;
        default:
          Billboard.error("Oh dear, that didn't work: " + rejection.status);
      }

      return $q.reject(rejection);
    }
  };
});
