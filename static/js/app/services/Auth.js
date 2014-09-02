/*global angular */

angular.module('app.services')
.factory('Auth', function ($http) {
  'use strict';

  var Auth = {};

  function extractData(response) {
    return response.data;
  }

  Auth.check = function () {
    return $http.get('/auth/check')
      .then(extractData)
      .then(function (authData) {
        return authData.logged_in;
      });
  };

  return Auth;
});
