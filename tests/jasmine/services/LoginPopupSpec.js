'use strict';

describe('LoginPopup', function () {
  beforeEach(module('app.services'));

  var FakePopup,
      FakeBillboard,
      LoginPopup;

  beforeEach(module(function ($provide) {
    $provide.factory('Popup', function ($q) {
      FakePopup = {};
      FakePopup.open = jasmine.createSpy('open').and.returnValue($q.when('fake'));
      return FakePopup;
    });

    FakeBillboard = jasmine.createSpyObj('Billboard', ['success']);
    $provide.value('Billboard', FakeBillboard);
  }));

  beforeEach(inject(function (_LoginPopup_) {
    LoginPopup = _LoginPopup_;
  }));

  describe('.open', function () {
    it('should open a popup for /login_popup', function () {
      LoginPopup.open();

      expect(FakePopup.open.calls.mostRecent().args[0]).toBe('/login_popup');
    });

    it('should return a promise', function () {
      var popup = LoginPopup.open();

      expect(popup).toBeAPromise();
    });

    it('should pass through options to Popup.open', function () {
      LoginPopup.open({width: 800, height: 600});

      expect(FakePopup.open).toHaveBeenCalledWith('/login_popup', {width: 800, height: 600});
    });

    describe('with a success message', function () {
      var fakePopupDeferred, $rootScope;

      beforeEach(inject(function ($q, _$rootScope_) {
        $rootScope = _$rootScope_;

        fakePopupDeferred = $q.defer();
        FakePopup.open.and.returnValue(fakePopupDeferred.promise);

        LoginPopup.open({successMessage: 'yay it worked'});
      }));

      it('should display the message when the popup closes', function () {
        fakePopupDeferred.resolve('done');
        $rootScope.$digest();

        expect(FakeBillboard.success).toHaveBeenCalledWith('yay it worked', jasmine.any(Object));
      });

      it('should dismiss the message after a while', function () {
        fakePopupDeferred.resolve('done');
        $rootScope.$digest();

        expect(FakeBillboard.success).toHaveBeenCalledWith(jasmine.any(String), jasmine.objectContaining({timeout: jasmine.any(Number)}));
      });
    });
  });
});
