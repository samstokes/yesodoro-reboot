/*global angular */

angular.module('app.controllers')
.controller('TasksCtrl', function ($scope, Task, Tasks, tasks, $timeout) {
  'use strict';

  $scope.tasks = tasks;
  $scope.tasks.todoToday = function todoToday() {
    return this.filter(function (task) {
      return task.isTodoToday();
    });
  };
  $scope.tasks.postponed = function postponed() {
    return this.filter(function (task) {
      return task.isPostponed();
    });
  };
  $scope.tasks.paused = function paused() {
    return this.filter(function (task) {
      return task.isPaused();
    });
  };
  $scope.tasks.done = function done() {
    return this.filter(function (task) {
      return task.isDone();
    });
  };
  $scope.tasks.sort(function (task1, task2) {
    var order1 = task1.task.order, order2 = task2.task.order;
    if (order1 < order2) return -1;
    if (order1 > order2) return 1;
    return 0;
  });


  $scope.newTask = new Task();

  $scope.estimatedRemaining = function estimatedRemaining() {
    if ($scope.tasks === undefined) return 0;
    var sum = 0;
    $scope.tasks.todoToday().forEach(function (task) {
      sum += task.estimatedRemaining();
    });
    return sum;
  };
  $scope.estimatedTotal = function estimatedTotal() {
    if ($scope.tasks === undefined) return 0;
    var sum = 0;
    $scope.tasks.todoToday().forEach(function (task) {
      sum += task.estimatedPomos();
    });
    return sum;
  };

  $scope.sortableOptions = {
    handle: '.reorder-handle',
    start: function (event, ui) {
      var startPos = ui.item.index();
      ui.item.data('startPos', startPos);
    },
    update: function (event, ui) {
      var startPos = ui.item.data('startPos'),
          endPos = ui.item.index(),
          taskToMove = $scope.tasks[startPos],
          taskAtEnd = $scope.tasks[endPos],

          /*
           * ui-sortable gets its item indices from the ng-model we specify,
           * which is the overall tasks list, so startPos and endPos are
           * indices into that list.  However, what we're sorting is just the
           * tasks that are todo today, so to calculate the delta we need the
           * tasks' indices into just the todo tasks.
           */
          todoTasks = $scope.tasks.todoToday(),
          todoIndexStart = todoTasks.indexOf(taskToMove),
          todoIndexEnd = todoTasks.indexOf(taskAtEnd),

          delta = todoIndexEnd - todoIndexStart;

      if (todoIndexStart < 0 || todoIndexEnd < 0) {
        taskToMove.broken = true;
        throw "Something got out of sync, couldn't reorder tasks!";
      }

      $scope.reorderTask(taskToMove, delta);
    }
  };

  $scope.postponedOrder = function (task) {
    return new Date(task.task.scheduled_for);
  };
  $scope.pausedOrder = $scope.postponedOrder;
  $scope.doneOrder = function (task) {
    return new Date(task.task.done_at);
  };

  $scope.addNewTask = function () {
    var task = this.newTask;
    this.newTask = new Task();

    var self = this;
    Tasks.create(task.task)
      .then(function (created) {
        task.id = created.id;
        task.task = created.task;
        task.notes = created.notes;

        self.appendTask(task);
      }, function () {
        var index = self.tasks.indexOf(task);
        if (index >= 0) {
          self.tasks.splice(index, 1);
        }
        if (!self.newTask.task.title) {
          self.newTask = task;
        }
      });
    return task;
  };


  $scope.appendTask = function (task) {
    this.tasks.push(task);
  };

  $scope.removeTask = function (task) {
    var index = this.tasks.indexOf(task);
    if (index > -1) {
      this.tasks.splice(index, 1);
    }
  };


  $scope.reorderTask = function (task, delta) {
    Tasks.reorder(task, delta)
      // TODO don't noop, apply updated task
      .then(undefined, function () {
        task.broken = true;
      });
  };
});
