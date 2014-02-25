'use strict';

angular.module('app.models')
.factory('Task', function (defaultTaskSchedule) {
  function Task(taskData) {
    if (undefined === taskData) {
      taskData = {};
    }

    this.id = taskData.id || '_new';
    this.task = taskData.task || {
      schedule: defaultTaskSchedule
    };
    this.ext_task = taskData.ext_task;
    this.notes = taskData.notes || [];
    this.estimates = taskData.estimates || [];
  }

  mergeInto(Task.prototype, (function () {
    function endOfDay(date) {
      date.setHours(23);
      date.setMinutes(59);
      date.setSeconds(59);
      return date;
    }

    return {
      isScheduledForToday: function isScheduledForToday() {
        var today = endOfDay(new Date());
        var scheduledFor = new Date(this.task.scheduled_for);
        return scheduledFor <= today;
      },

      isTodoToday: function isTodoToday() {
        return this.task.active && !this.task.done_at && this.isScheduledForToday();
      },

      isOverdue: function isOverdue() {
        var yesterday = endOfDay(new Date());
        // surprisingly, this does the right thing for day 1 (rolls back month)
        yesterday.setDate(yesterday.getDate() - 1);
        var scheduledFor = new Date(this.task.scheduled_for);
        return this.task.active && !this.task.done_at && scheduledFor <= yesterday;
      },
      isPostponed: function isPostponed() {
        return this.task.active && !this.task.done_at && !this.isScheduledForToday();
      },
      isPaused: function isPaused() {
        return !this.task.active && !this.task.done_at;
      },
      isDone: function isDone() {
        return !!this.task.done_at;
      },

      newEstimate: function (pomos) {
        return {estimate: {pomos: pomos, task_id: this.id}};
      },

      addEstimate: function (estimate) {
        var length = this.estimates.push(estimate);
        return length - 1;
      },
      removeEstimate: function (index) {
        return this.estimates.splice(index, 1);
      },

      estimatedPomos: function () {
        if (this.estimates.length == 0) return 0;
        var estimate = this.estimates[0];
        return estimate.estimate.pomos;
      },
      estimatedRemaining: function () {
        return Math.max(0,
          this.estimatedPomos() - this.task.pomos);
      }
    };
  }()));

  return Task;
});
