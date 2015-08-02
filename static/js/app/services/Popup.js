/*global
   angular
 , mergeInto
 */

angular.module('app.services')
.factory('Popup', function ($rootScope, $window, $q, $log) {
  'use strict';

  var Popup = {};

  var POPUP_NAME = 'popup';

  function optionsToFeaturesString(userOptions) {
    var DEFAULT_OPTIONS = {
      menubar: false,
      toolbar: false,
      location: false
    };
    var options = mergeInto(DEFAULT_OPTIONS, userOptions);

    var features = [];

    for (var option in options) {
      if (options.hasOwnProperty(option)) {
        var value = options[option];
        switch (typeof(value)) {
        case 'number':
          break;
        case 'boolean':
          value = value ? 1 : 0;
          break;
        default:
          throw new Error('unexpected ' + typeof(value) + ' for option ' + option);
        }

        features.push(option + '=' + value);
      }
    }

    return features.join(',');
  }

  Popup.open = function open (url, options) {
    var featuresString = optionsToFeaturesString(options);

    var deferred = $q.defer();

    $window.$setPopupResult = function $setPopupResult (result) {
      $rootScope.$apply(function () {
        deferred.resolve(result);
      });
    };

    $window.$cancelPopup = function $cancelPopup (reason) {
      $rootScope.$apply(function () {
        deferred.reject(reason);
      });
    };

    function cleanup() {
      delete $window.$setPopupResult;
      delete $window.$cancelPopup;
    }

    $window.open(url, POPUP_NAME, featuresString);
    $log.info('Opened popup:', url);

    var popup = deferred.promise;

    popup.always(cleanup);
    popup.then(
      function popupCompleted(result) {
        $log.info('Popup completed', url, result);
      },
      function popupCancelled(reason) {
        $log.info('Popup cancelled', url, reason);
      }
    );

    return popup;
  };

  return Popup;
});
