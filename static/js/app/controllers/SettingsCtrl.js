/*global angular */

angular.module('app.controllers')
.controller('SettingsCtrl', function ($scope, loggedIn) {
  'use strict';

  $scope.loggedIn = loggedIn;
});
