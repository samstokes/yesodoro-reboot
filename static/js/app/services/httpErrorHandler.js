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
          Billboard.error("Maybe your XSRF token expired?", {
            action: {
              reload: true,
              message: 'RELOAD'
            }
          });
          break;
        case 401:
          Billboard.error('Oops tiem to login', {
            action: {
              url: '/login_popup',
              message: 'MOAR LOGIN!',
              onCloseMessage: 'TRY AGANE'
            }
          });
          break;
        case 500:
          Billboard.error("Something bad happened!");
          break;
        case 0:
          Billboard.error("Looks like the server is down maybe?");
          break;
        default:
          Billboard.error("I don't even: " + rejection.status);
      }

      return $q.reject(rejection);
    }
  };
});
