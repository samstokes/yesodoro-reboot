/*global angular */

angular.module('app.controllers')
.controller('TasksCtrl', function ($scope, Task, Tasks, $timeout, messageFromServer, Billboard, $window /* TODO */) {
  'use strict';

  /*
   * If the server set a message before we loaded the app, display it via
   * Billboard.  Putting this here is a hack, but we need it somewhere that
   * runs on app load, into which we can inject Billboard and
   * messageFromServer.
   */
  if (messageFromServer) {
    Billboard.success(messageFromServer, {timeout: 10000});
  }

  /* TODO hack - pull params out of the URL */
  var daysMatch = $window.location.toString().match(/\?days=(\d+)/),
      params = {};
  if (daysMatch !== null) {
    params.days = daysMatch[1];
  }

  Tasks.all(params).then(function (tasks) {
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
  });


  $scope.newTask = new Task();

  $scope.estimatedRemaining = function estimatedRemaining() {
    var sum = 0;
    $scope.tasks.todoToday().forEach(function (task) {
      sum += task.estimatedRemaining();
    });
    return sum;
  };
  $scope.estimatedTotal = function estimatedTotal() {
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

        self.tasks.splice(self.tasks.length, 0, task);
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

  $scope.removeTask = function (task) {
    var index = this.tasks.indexOf(task);
    if (index > -1) {
      this.tasks.splice(index, 1);
    }
  };

  $scope.completeTask = function (task) {
    task.going = true;
    var self = this;

    Tasks.complete(task.id)
      .then(function (data) {
        if (data.completed) {
          task.going = false;
          task.task = data.completed.task;
        } else {
          task.going = false;
          task.broken = true;
        }

        if (data.recurred) {
          data.recurred.notes = [];
          self.tasks.push(data.recurred);
        }
      }, function () {
        task.going = false;
        task.broken = true;
      });
  };

  $scope.restartTask = function (task) {
    task.going = true;

    Tasks.restart(task.id)
      .then(function (restarted) {
        task.going = false;
        task.task = restarted.task;
      }, function () {
        task.going = false;
        task.broken = true;
      });
  };

  $scope.postponeTask = function (task) {
    task.going = true;

    Tasks.postpone(task.id)
      .then(function (postponed) {
        task.going = false;
        task.task = postponed.task;
      }, function () {
        task.going = false;
        task.broken = true;
      });
  };

  $scope.unpostponeTask = function (task) {
    task.going = true;

    Tasks.unpostpone(task.id)
      .then(function (unpostponed) {
        task.going = false;
        task.task = unpostponed.task;
      }, function () {
        task.going = false;
        task.broken = true;
      });
  };

  $scope.pauseTask = function (task) {
    task.going = true;

    Tasks.pause(task.id)
      .then(function (paused) {
        task.going = false;
        task.task = paused.task;
      }, function () {
        task.going = false;
        task.broken = true;
      });
  };

  $scope.unpauseTask = function (task) {
    task.going = true;

    Tasks.unpause(task.id)
      .then(function (unpaused) {
        task.going = false;
        task.task = unpaused.task;
      }, function () {
        task.going = false;
        task.broken = true;
      });
  };

  $scope.updateTask = function (task) {
    Tasks.update(task)
      // TODO don't noop, apply updated task
      .then(undefined, function () {
        task.broken = true;
      });
  };

  $scope.reorderTask = function (task, delta) {
    Tasks.reorder(task, delta)
      // TODO don't noop, apply updated task
      .then(undefined, function () {
        task.broken = true;
      });
  };

  $scope.deleteTask = function (task) {
    task.going = true;
    var self = this;

    Tasks.delete(task.id)
      .then(function () {
        self.removeTask(task);
      }, function () {
        task.going = false;
        task.broken = true;
      });
  };

  $scope.addEstimate = function (task, pomos) {
    Tasks.addEstimate(task, pomos)
      .then(undefined, function () {
        task.broken = true;
      });
  };

  $scope.logPomo = function (task) {
    Tasks.logPomos(task, 1)
      .then(undefined, function () {
        task.broken = true;
      });
  };


  $scope.showNotes = function (task) {
    task.notes.isVisible = true;
  };

  $scope.hideNotes = function (task) {
    $timeout(function () {
      task.notes.isVisible = false;
    }, 0);
  };
});
