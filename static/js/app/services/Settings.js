/*global angular */

angular.module('app.services')
.factory('Settings', function ($http) {
  'use strict';

  var Settings = {};

  function extractData(response) {
    return response.data;
  }

  Settings.isAuthed = function isAuthed() {
    return $http.get('/auth/check')
      .then(extractData)
      .then(function (authData) {
        return authData.logged_in;
      });
  };

  Settings.syncTimezone = function syncTimezone() {
    return $http.put('/settings/timezone', {
      offset: getTimezoneOffset()
    }).then(function (response) {
      return response.data && !!response.data.updated;
    });
  };

  function getTimezoneOffset() {
    return -new Date().getTimezoneOffset();
  }

  return Settings;
});
