'use strict';

describe('Task', function () {
  var Task;

  angular.module('TestApp', ['app.models'])
    .value('defaultTaskSchedule', 'Once');

  beforeEach(module('TestApp'));

  beforeEach(inject(function (_Task_) {
    Task = _Task_;
  }));

  describe('constructed from blank', function () {
    var task;
    beforeEach(function () { task = new Task(); });

    it('should have no title', function () {
      expect(task.task.title).toBeUndefined();
    });

    it('should be a one-off task', function () {
      expect(task.task.schedule).toBe('Once');
    });

    it('should be todo today', function () {
      expect(task.isTodoToday()).toBeTruthy();
    });

    it('should not be overdue', function () {
      expect(task.isOverdue()).toBeFalsy();
    });

    it('should not be postponed', function () {
      expect(task.isPostponed()).toBeFalsy();
    });

    it('should not be paused', function () {
      expect(task.isPaused()).toBeFalsy();
    });

    it('should not be done', function () {
      expect(task.isDone()).toBeFalsy();
    });

    it('should have no external task link', function () {
      expect(task.ext_task).toBeUndefined();
    });

    it('should have no notes', function () {
      expect(task.notes).toBeEmpty();
    });

    it('should have a zero estimate', function () {
      expect(task.estimatedPomos()).toBe(0);
    });

    it('should have zero estimated pomos remaining', function () {
      expect(task.estimatedRemaining()).toBe(0);
    });
  });


  describe('dayDone date formatting', function () {
    it('should return YYYY-MM-DD', function () {
      var task = new Task({task: {done_at: '2014-08-04T14:41Z'}});

      expect(task.dayDone()).toEqual('2014-08-04');
    });

    it('should be timezone aware', function () {
      // relies on Hawaii timezone (-10) as forced in config/karma.conf.js

      var task = new Task({task: {done_at: '2014-08-04T09:41Z'}});

      expect(task.dayDone()).toEqual('2014-08-03');
    });
  });
});
