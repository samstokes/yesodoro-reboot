'use strict';

describe('Auth', function () {
  var Auth, $httpBackend;

  beforeEach(module('app.services'));

  beforeEach(inject(function (_$httpBackend_, _Auth_) {
    $httpBackend = _$httpBackend_;
    Auth = _Auth_;
  }));

  afterEach(function () {
    $httpBackend.verifyNoOutstandingExpectation();
  });

  describe('.check()', function () {
    beforeEach(function () {
      $httpBackend.whenGET('/auth/check').respond({
        logged_in: true
      });
    });

    it('should check with the server if the user is logged in', function () {
      $httpBackend.expectGET('/auth/check');
      Auth.check();
    });

    it('should return a promise of the login status', function () {
      var status = Auth.check();
      expect(status.then).toBeDefined();

      var loggedIn;
      status.then(function (_loggedIn) { loggedIn = _loggedIn; });
      $httpBackend.flush();

      expect(loggedIn).toBe(true);
    });
  });
});
