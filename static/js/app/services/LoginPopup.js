/*global
   angular
 */

angular.module('app.services')
.factory('LoginPopup', function (Popup, Billboard) {
  'use strict';

  var LOGIN_POPUP_URL = '/login_popup';

  var LoginPopup = {};

  LoginPopup.open = function open (options) {
    var successMessage;
    if (options && options.successMessage) {
      successMessage = options.successMessage;
      delete options.successMessage;
    }

    return Popup.open(LOGIN_POPUP_URL, options)
      .then(function loginCompleted() {
        if (successMessage) {
          Billboard.success(successMessage, {timeout: 10000});
        }
      });
  };

  return LoginPopup;
});
