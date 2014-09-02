'use strict';

describe('SettingsCtrl', function () {
  var scope, ctrl;

  beforeEach(module('app.controllers'));

  beforeEach(module(function ($provide) {
    $provide.value('loggedIn', true);
  }));

  beforeEach(inject(function ($controller) {
    scope = {};
    ctrl = $controller('SettingsCtrl', {$scope: scope});
  }));

  it('should expose the login status in the scope', function () {
    expect(scope.loggedIn).toBe(true);
  });
});
