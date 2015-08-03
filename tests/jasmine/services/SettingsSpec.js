'use strict';

describe('Settings', function () {
  var Settings, $httpBackend;

  beforeEach(module('app.services'));

  beforeEach(inject(function (_$httpBackend_, _Settings_) {
    $httpBackend = _$httpBackend_;
    Settings = _Settings_;
  }));

  afterEach(function () {
    $httpBackend.verifyNoOutstandingExpectation();
  });

  describe('.isAuthed()', function () {
    beforeEach(function () {
      $httpBackend.whenGET('/auth/check').respond({
        logged_in: true
      });
    });

    it('should check with the server if the user is logged in', function () {
      $httpBackend.expectGET('/auth/check');
      Settings.isAuthed();
    });

    it('should return a promise of the login status', function () {
      var status = Settings.isAuthed();
      expect(status).toBeAPromise();

      var loggedIn;
      status.then(function (_loggedIn) { loggedIn = _loggedIn; });
      $httpBackend.flush();

      expect(loggedIn).toBe(true);
    });
  });

  describe('.syncTimezone()', function () {
    it('should return a promise of truth if we updated the timezone on the server', function () {
      $httpBackend.expectPUT('/settings/timezone').respond({
        updated: '-0400'
      });

      var pUpdated = Settings.syncTimezone();
      expect(pUpdated).toBeAPromise();

      var updated;
      pUpdated.then(function (_updated) { updated = _updated; });
      $httpBackend.flush();

      expect(updated).toBe(true);
    });

    it('should return a promise of falsehood if the server timezone was already in sync', function () {
      $httpBackend.expectPUT('/settings/timezone').respond({
        updated: false
      });

      var pUpdated = Settings.syncTimezone();
      expect(pUpdated).toBeAPromise();

      var updated;
      pUpdated.then(function (_updated) { updated = _updated; });
      $httpBackend.flush();

      expect(updated).toBe(false);
    });
  });
});
