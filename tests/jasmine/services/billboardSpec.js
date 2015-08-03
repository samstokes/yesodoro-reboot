'use strict';

describe('Billboard service', function () {
  beforeEach(module('app.services'));

  var $rootScope,
      Billboard,
      lastEvent;

  beforeEach(inject(function(_$rootScope_, _Billboard_) {
    $rootScope = _$rootScope_;
    Billboard = _Billboard_;

    Billboard.watch(function (event) {
      lastEvent = event;
    });
  }));

  it('should notify watchers of successes', function () {
    Billboard.success('woohoo!');
    $rootScope.$apply();

    expect(lastEvent).not.toBeFalsy();
    expect(lastEvent.message).toBe('woohoo!');
    expect(lastEvent.severity).toBe('success');
  });

  it('should notify watchers of errors', function () {
    Billboard.error('oh noes!');
    $rootScope.$apply();

    expect(lastEvent).not.toBeFalsy();
    expect(lastEvent.message).toBe('oh noes!');
    expect(lastEvent.severity).toBe('error');
  });

  it('should support a more verbose syntax for notifying', function () {
    Billboard.notify('success', 'woohoo!');
    $rootScope.$apply();

    expect(lastEvent).not.toBeFalsy();
    expect(lastEvent.message).toBe('woohoo!');
    expect(lastEvent.severity).toBe('success');
  });

  it('should error on undefined severities', function () {
    expect(function () {
      Billboard.notify('info', 'Good news, everyone!');
    }).toThrow();
  });

  it('should error if severity is null', function () {
    expect(function () {
      Billboard.notify(null, 'Kaboom');
    }).toThrow();
  });

  it('should error if message is null', function () {
    expect(function () {
      Billboard.notify('error', null);
    }).toThrow();
  });

  describe('.clear', function () {
    it('should clear the last event', function () {
      Billboard.success('woohoo!');
      $rootScope.$apply();
      Billboard.clear();
      $rootScope.$apply();

      expect(lastEvent).toBeFalsy();
    });

    it('should not clobber subsequent events if passed the event to clear', function () {
      var event = Billboard.success('woohoo!');
      $rootScope.$apply();
      Billboard.error('meanwhile, this happened');
      $rootScope.$apply();
      Billboard.clear(event);
      $rootScope.$apply();

      expect(lastEvent).not.toBeFalsy();
      expect(lastEvent.severity).toBe('error');
    });
  });

  describe('transient events', function () {
    var $timeout;
    beforeEach(inject(function (_$timeout_) {
      $timeout = _$timeout_;
    }));

    it('should clear the event after the timeout expires', function () {
      Billboard.success('Logged in', {timeout: 5000});
      $rootScope.$apply();
      expect(lastEvent.message).toBe('Logged in');

      $timeout.flush();

      expect(lastEvent).toBeFalsy();
    });

    it('should not clobber subsequent events', function () {
      Billboard.success('Logged in', {timeout: 5000});
      $rootScope.$apply();
      Billboard.error('Oops, logged out again');
      $rootScope.$apply();

      $timeout.flush();

      expect(lastEvent).not.toBeFalsy();
      expect(lastEvent.severity).toBe('error');
    });
  });

  describe('actions', function () {
    describe('popup', function () {
      it('should allow specifying a popup URL to respond to the event', function () {
        Billboard.error('Unauthorised', {
          action: {message: 'Try logging in again', url: '/login'}
        });
        $rootScope.$apply();

        expect(lastEvent.action).not.toBeFalsy();
        expect(lastEvent.action.message).toBe('Try logging in again');
        expect(lastEvent.action.url).toBe('/login');
      });

      it('should set a default message if not specified', function () {
        Billboard.error('Unauthorised', {action: {url: '/login'}});
        $rootScope.$apply();

        expect(lastEvent.action.message).not.toBeFalsy();
        expect(lastEvent.action.message).not.toBeEmpty();
      });

      it('should allow setting a message for when the popup closes', function () {
        Billboard.error('Unauthorised', {
          action: {url: '/login', onCloseMessage: 'Done!'}
        });
        $rootScope.$apply();

        expect(lastEvent.action.onCloseMessage).toBe('Done!');
      });

      it('should not allow setting an external URL', function () {
        [
          'https://www.google.com',
          'http://samstokes.co.uk',
          '//www.google.com'
        ].forEach(function (badUrl) {
          expect(function () {
            Billboard.error('Boing', {action: {url: badUrl}});
          }).toThrow();
        });
      });

      it('should error if you try to set a successMessage', function () {
        expect(function () {
          Billboard.error('Unauthorised', {action: {url: '/login', successMessage: 'Try again'}});
        }).toThrow();
      });
    });

    describe('login', function () {
      it('should allow showing a login link to resolve the error', function () {
        Billboard.error('Unauthorised', {
          action: {message: 'Try logging in again', login: true}
        });
        $rootScope.$apply();

        expect(lastEvent.action).not.toBeFalsy();
        expect(lastEvent.action.message).toBe('Try logging in again');
      });

      it('should error if you try to set login: false', function () {
        [false, 0, null, undefined].forEach(function (falsy) {
          expect(function () {
            Billboard.error('Unauthorised', {action: {login: falsy}});
          }).toThrow();
        });
      });

      it('should set a default message and successMessage if not specified', function () {
        Billboard.error('Unauthorised', {action: {login: true}});
        $rootScope.$apply();

        expect(lastEvent.action.message).not.toBeFalsy();
        expect(lastEvent.action.message).not.toBeEmpty();

        expect(lastEvent.action.successMessage).not.toBeFalsy();
        expect(lastEvent.action.successMessage).not.toBeEmpty();
      });

      it('should allow setting a message for after login succeeded', function () {
        Billboard.error('Unauthorised', {
          action: {login: true, successMessage: 'Done!'}
        });
        $rootScope.$apply();

        expect(lastEvent.action.successMessage).toBe('Done!');
      });

      it('should error if you try to set an onCloseMessage', function () {
        expect(function () {
          Billboard.error('Unauthorised', {action: {login: true, onCloseMessage: 'Try again'}});
        }).toThrow();
      });
    });

    describe('reload', function () {
      it('should allow specifying a reload action', function () {
        Billboard.error('token expired', {
          action: {message: 'Try refreshing', reload: true}
        });
        $rootScope.$apply();

        expect(lastEvent.action.reload).toBe(true);
      });

      it('should error if you try to set reload: false', function () {
        [false, 0, null, undefined].forEach(function (falsy) {
          expect(function () {
            Billboard.error('Unauthorised', {action: {reload: falsy}});
          }).toThrow();
        });
      });

      it('should error if you try to set an onCloseMessage', function () {
        expect(function () {
          Billboard.error('Unauthorised', {action: {reload: true, onCloseMessage: 'Try again'}});
        }).toThrow();
      });

      it('should error if you try to set a successMessage', function () {
        expect(function () {
          Billboard.error('Unauthorised', {action: {reload: true, successMessage: 'Try again'}});
        }).toThrow();
      });
    });

    describe('option sanity checking', function () {
      it('should error if none of reload, login or url is specified', function () {
        expect(function () {
          Billboard.error('Unauthorised', {action: {message: 'Good luck with that'}});
        }).toThrow();
      });

      it('should error if both reload and url are specified', function () {
        expect(function () {
          Billboard.error('Unauthorised', {
            action: {
              reload: true,
              url: '/login'
            }
          });
        }).toThrow();
      });

      it('should error if both reload and login are specified', function () {
        expect(function () {
          Billboard.error('Unauthorised', {
            action: {
              reload: true,
              login: true
            }
          });
        }).toThrow();
      });

      it('should error if both url and login are specified', function () {
        expect(function () {
          Billboard.error('Unauthorised', {
            action: {
              url: '/login',
              login: true,
            }
          });
        }).toThrow();
      });
    });
  });
});
