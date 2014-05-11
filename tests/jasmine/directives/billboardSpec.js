'use strict';

describe('billboard directive', function () {
  beforeEach(module('app.directives'));

  var $window,
      $compile,
      $rootScope,
      Billboard,
      elem;

  beforeEach(module(function ($provide) {
    $provide.factory('$window', function () {
      $window = angular.mock.createMockWindow();
      return $window;
    });
  }));

  beforeEach(inject(function (_$compile_, _$rootScope_, _Billboard_) {
    $compile = _$compile_;
    $rootScope = _$rootScope_;
    Billboard = _Billboard_;

    elem = $compile('<billboard/>')($rootScope);
  }));

  describe('with no event', function () {
    beforeEach(function () {
      $rootScope.$digest();
    });

    it('should have class .billboard', function () {
      expect(elem).toHaveClass('billboard');
    });

    it('should have class .empty', function () {
      expect(elem).toHaveClass('empty');
    });

    it('should have no visible children', function () {
      expect(elem.children().filter(function () {
        return $(this).css('display') !== 'none';
      })).toBeEmpty();
    });
  });

  describe('displaying a success', function () {
    beforeEach(function () {
      Billboard.success('Everything is awesome');
      $rootScope.$digest();
    });

    it('should have class .billboard', function () {
      expect(elem).toHaveClass('billboard');
    });

    it('should not have class .empty', function () {
      expect(elem).not.toHaveClass('empty');
    });

    it('should have class .success', function () {
      expect(elem).toHaveClass('success');
    });
  });

  describe('displaying an error', function () {
    beforeEach(function () {
      Billboard.error('Everything is wrong');
      $rootScope.$digest();
    });

    it('should have class .billboard', function () {
      expect(elem).toHaveClass('billboard');
    });

    it('should not have class .empty', function () {
      expect(elem).not.toHaveClass('empty');
    });

    it('should have visible children', function () {
      expect(elem.children().filter(function () {
        return $(this).css('display') !== 'none';
      })).not.toBeEmpty();
    });

    it('should have class .error', function () {
      expect(elem).toHaveClass('error');
    });

    it('should display the message', function () {
      var message = elem.find('.message');
      expect(message).not.toBeEmpty();
      expect(message.text()).toBe('Everything is wrong');
    });

    it('should dismiss the error when you click the X', function () {
      var x = elem.find('.dismiss');
      expect(x).not.toBeEmpty();

      x.click();

      expect(elem).toHaveClass('empty');
      expect(elem.text()).not.toContain('Everything is wrong');
    });
  });

  describe('with a popup action', function () {
    var action;

    beforeEach(function () {
      $window.open = function fakeOpen() {};
      spyOn($window, 'open');

      Billboard.error('Kaboom', {
        action: {
          message: 'Click to fix',
          url: '/fix'
        }
      });
      $rootScope.$digest();

      action = elem.find('.action');
    });

    it('should display the action message with class .action', function () {
      expect(action).not.toBeEmpty();
      expect(action.text()).toBe('Click to fix');
    });

    it('should popup the url when you click the action', function () {
      action.click();
      expect($window.open).toHaveBeenCalled();
      expect($window.open.calls.mostRecent().args[0]).toBe('/fix');
    });

    it('should dismiss the error when you click the action', function () {
      action.click();
      expect(elem).toHaveClass('empty');
      expect(elem.text()).not.toContain('Kaboom');
    });

    describe('with an onCloseMessage', function () {
      var $timeout,
          fakePopup;

      beforeEach(inject(function (_$timeout_) {
        $timeout = _$timeout_;

        fakePopup = {};
        $window.open.and.returnValue(fakePopup);

        Billboard.error('Kaboom', {
          action: {
            message: 'Click to fix',
            url: '/fix',
            onCloseMessage: 'Fixed that for you'
          }
        });
        $rootScope.$digest();

        action.click();

        expect(fakePopup.onunload).toBeDefined();
        fakePopup.onunload();
        $rootScope.$digest();
      }));

      it('should display the message when the popup closes', function () {
        var message = elem.find('.message');
        expect(message).not.toBeEmpty();
        expect(message.text()).toBe('Fixed that for you');
      });

      it('should dismiss the message after a while', function () {
        $timeout.flush();

        expect(elem).toHaveClass('empty');
        expect(elem.text()).not.toContain('Fixed that for you');
      });
    });
  });

  describe('with a reload action', function () {
    var action;

    beforeEach(function () {
      $window.location = jasmine.createSpyObj('location', ['reload']);

      Billboard.error('Kaboom', {
        action: {
          message: 'Click to fix',
          reload: true
        }
      });
      $rootScope.$digest();

      action = elem.find('.action');
    });

    it('should display the action message with class .action', function () {
      expect(action).not.toBeEmpty();
      expect(action.text()).toBe('Click to fix');
    });

    it('should reload the page when you click the action', function () {
      action.click();
      expect($window.location.reload).toHaveBeenCalled();
    });

    it('should dismiss the error when you click the action', function () {
      action.click();
      expect(elem).toHaveClass('empty');
      expect(elem.text()).not.toContain('Kaboom');
    });
  });
});
