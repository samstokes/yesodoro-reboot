'use strict';

describe('Popup', function () {
  beforeEach(module('app.services'));

  var $window,
      Popup;

  beforeEach(module(function ($provide) {
    $window = {};
    $window.open = jasmine.createSpy('open');

    $provide.value('$window', $window);
  }));

  beforeEach(inject(function (_Popup_) {
    Popup = _Popup_;
  }));

  describe('.open', function () {
    it('should invoke window.open with the url', function () {
      Popup.open('/foo');

      expect($window.open).toHaveBeenCalledWith('/foo', jasmine.any(String), jasmine.any(String));
    });

    it('should default to a minimal popup', function () {
      $window.open.and.callFake(function (url, name, props) {
        expect(props).toMatch(/\bmenubar=0\b/);
        expect(props).toMatch(/\btoolbar=0\b/);
        expect(props).toMatch(/\blocation=0\b/);
      });

      Popup.open('/foo');

      expect($window.open).toHaveBeenCalled();
    });

    it('should allow passing numeric properties', function () {
      $window.open.and.callFake(function (url, name, props) {
        expect(props).toMatch(/\bwidth=800\b/);
        expect(props).toMatch(/\bheight=600\b/);
      });

      Popup.open('/foo', {width: 800, height: 600});

      expect($window.open).toHaveBeenCalled();
    });

    it('should allow passing boolean properties', function () {
      $window.open.and.callFake(function (url, name, props) {
        expect(props).toMatch(/\bmenubar=1\b/);
      });

      Popup.open('/foo', {menubar: true});

      expect($window.open).toHaveBeenCalled();
    });

    it('should throw an exception for unexpected option types', function () {
      expect(function () {
        Popup.open('/foo', {widget: {foo: 'bar'}});
      }).toThrowError(/widget/);
    });

    it('should add a $window.$setPopupResult function for the popup to call via window.opener', function () {
      Popup.open('/foo');

      expect(typeof($window.$setPopupResult)).toBe('function');
    });

    it('should return a promise and fulfil it when $setPopupResult is called', function () {
      var popup = Popup.open('/foo');

      expect(popup.then).toBeDefined();

      var result;
      popup.then(function (_result) { result = _result; });

      // pretend I am the popup calling this via window.opener
      $window.$setPopupResult(42);

      expect(result).toBe(42);
    });

    it('should add a $window.$cancelPopup function for the popup to call via window.opener', function () {
      Popup.open('/foo');

      expect(typeof($window.$cancelPopup)).toBe('function');
    });

    it('should reject the promise if $cancelPopup is called', function () {
      var popup = Popup.open('/foo');

      expect(popup.then).toBeDefined();

      var reason;
      popup.then(null, function (_reason) { reason = _reason; });

      // pretend I am the popup calling this via window.opener
      $window.$cancelPopup('fail');

      expect(reason).toBe('fail');
    });

    it('should clean up $window.$setPopupResult and $window.$cancelPopup once $setPopupResult is called', function () {
      Popup.open('/foo');
      expect(typeof($window.$setPopupResult)).toBe('function');
      expect(typeof($window.$cancelPopup)).toBe('function');

      $window.$setPopupResult(42);

      expect($window.$setPopupResult).toBeUndefined();
      expect($window.$cancelPopup).toBeUndefined();
    });

    it('should clean up $window.$setPopupResult and $window.$cancelPopup once $cancelPopup is called', function () {
      Popup.open('/foo');
      expect(typeof($window.$setPopupResult)).toBe('function');
      expect(typeof($window.$cancelPopup)).toBe('function');

      $window.$cancelPopup('fail');

      expect($window.$setPopupResult).toBeUndefined();
      expect($window.$cancelPopup).toBeUndefined();
    });
  });
});
