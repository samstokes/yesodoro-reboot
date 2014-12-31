'use strict';

describe('filters', function () {
  beforeEach(module('app.filters'));

  var dateFromDay;

  beforeEach(inject(function (_dateFromDayFilter_) {
    dateFromDay = _dateFromDayFilter_;
  }));

  describe('dateFromDay', function () {
    it('should parse a YYYY-MM-DD format into a date', function () {
      var date = dateFromDay('2014-08-04');

      expect(date.getUTCFullYear()).toBe(2014);
      expect(date.getUTCMonth()).toBe(8 - 1); // JS months are 0-based
      expect(date.getUTCDate()).toBe(4);
    });

    it('should be timezone aware', function () {
      // relies on non-UTC timezone (e.g. as forced in config/karma.conf.js)
      // will always pass vacuously in UTC
      // (N.B. this tests .getDate() vs .getUTCDate() above)

      var date = dateFromDay('2014-08-04');

      expect(date.getFullYear()).toBe(2014);
      expect(date.getMonth()).toBe(8 - 1); // JS months are 0-based
      expect(date.getDate()).toBe(4);
    });
  });
});
