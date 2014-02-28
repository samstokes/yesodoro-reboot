'use strict';

angular.module('app.controllers')
.controller('TasksCtrl', function ($scope, Task, Tasks, $timeout) {
  $scope.tasks = Tasks.all();
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
      var start_pos = ui.item.index();
      ui.item.data('start_pos', start_pos);
    },
    update: function (event, ui) {
      var start_pos = ui.item.data('start_pos'),
          end_pos = ui.item.index(),
          delta = end_pos - start_pos;
      var task = $scope.tasks[start_pos];
      $scope.reorderTask(task, delta);
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
    this.tasks.splice(this.tasks.length, 0, task);
    Tasks.create(task.task)
      .then(function (created) {
        task.id = created.id;
        task.task = created.task;
        task.notes = created.notes;
      }, function () {
        task.broken = true;
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
})


.controller('DoneTasksCtrl', function ($scope) {
  var tasksByDay = {};

  function dateYMD(date) {
    return date.getFullYear() + '-' + (date.getMonth() + 1) + '-' + date.getDate();
  }

  $scope.dayDone = function (task) {
    if (!task.task.done_at) return null;
    return dateYMD(new Date(task.task.done_at))
  };
  $scope.doneSameDay = function (otherTask) {
    var otherDay = $scope.dayDone(otherTask);
    return function (task) {
      return $scope.dayDone(task) === otherDay;
    };
  };

  $scope.dayOrder = function (task) {
    return $scope.dayDone(task);
  };

  $scope.tasksToFilter = function () {
    // reset day index each time list is rendered
    tasksByDay = {};
    return $scope.tasks;
  };

  $scope.uniqueDay = function (task) {
    var day = $scope.dayDone(task);
    if (!day) return false;
    if (!tasksByDay.hasOwnProperty(day)) {
      tasksByDay[day] = task;
      return true;
    } else {
      return false;
    }
  };
});
