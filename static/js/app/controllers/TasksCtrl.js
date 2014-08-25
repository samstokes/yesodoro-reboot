/*global angular */

angular.module('app.controllers')
.controller('TasksCtrl', function ($scope, Task, Tasks, tasks, hibiUi, $timeout) {
  'use strict';

  var isOldUi = 'old' === hibiUi;

  $scope.today = new Date();
  $scope.dateFormat = 'EEEE, MMMM d';

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

  function calculateDeltaOld(startPos, endPos, taskToMove) {
    /*
     * ui-sortable gets its item indices from the ng-model we specify,
     * which is the overall tasks list, so startPos and endPos are
     * indices into that list.  However, what we're sorting is just the
     * tasks that are todo today, so to calculate the delta we need the
     * tasks' indices into just the todo tasks.
     */
    var taskAtEnd = $scope.tasks[endPos],
        todoTasks = $scope.tasks.todoToday(),
        todoIndexStart = todoTasks.indexOf(taskToMove),
        todoIndexEnd = todoTasks.indexOf(taskAtEnd),
        delta = todoIndexEnd - todoIndexStart;

    if (todoIndexStart < 0 || todoIndexEnd < 0) {
      taskToMove.broken = true;
      throw "Something got out of sync, couldn't reorder tasks!";
    }

    return delta;
  }

  function calculateDeltaNew(startPos, endPos, taskToMove) {
    return endPos - startPos;
  }

  var calculateDelta = isOldUi ? calculateDeltaOld : calculateDeltaNew;

  $scope.sortableOptions = {
    handle: '.reorder-handle',
    start: function (event, ui) {
      var startPos = ui.item.index();
      ui.item.data('startPos', startPos);
    },
    update: function (event, ui) {
      var startPos = ui.item.data('startPos'),
          endPos = ui.item.index(),
          taskToMove = ui.item.scope().task;

      if (!taskToMove)
        throw "Couldn't identify task to move! " + JSON.stringify(ui.item);

      var delta = calculateDelta(startPos, endPos, taskToMove);
      $scope.reorderTask(taskToMove, delta);
    }
  };

  $scope.taskOrder = function (task) {
    return task.task.order;
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
