/*global angular */

angular.module('app.directives')
.directive('hibiTask', function () {
  'use strict';

  return {
    restrict: 'A',
    scope: {
      task: '=hibiTask',
      onExtraTask: '&',
      onRemove: '&'
    },
    controller: 'HibiTaskCtrl',
    templateUrl: '/templates/taskTr',
    link: function (scope, elem, attrs) {
      elem.mouseenter(function () {
        scope.$apply(function () {
          scope.task.notes.isVisible = true;
        });
      });

      elem.mouseleave(function () {
        scope.$apply(function () {
          scope.task.notes.isVisible = false;
        });
      });
    }
  };
})
.controller('HibiTaskCtrl', function ($scope, Tasks) {
  'use strict';

  function removeTask() {
    $scope.onRemove();
  }

  function onError() {
    if (!$scope.task) return;

    if ($scope.task.going) $scope.task.going = false;
    $scope.task.broken = true;
  }

  $scope.complete = function () {
    this.task.going = true;

    var self = this;
    Tasks.complete(this.task.id)
      .then(function (data) {
        if (data.completed) {
          self.task.going = false;
          self.task.task = data.completed.task;
        } else {
          onError();
        }

        if (data.recurred) {
          data.recurred.notes = [];
          self.onExtraTask({extraTask: data.recurred});
        }
      }, onError);
  };

  $scope.restart = function () {
    this.task.going = true;

    var self = this;
    Tasks.restart(this.task.id)
      .then(function (restarted) {
        self.task.going = false;
        self.task.task = restarted.task;
      }, onError);
  };

  $scope.postpone = function () {
    this.task.going = true;

    var self = this;
    Tasks.postpone(this.task.id)
      .then(function (postponed) {
        self.task.going = false;
        self.task.task = postponed.task;
      }, onError);
  };

  $scope.unpostpone = function () {
    this.task.going = true;

    var self = this;
    Tasks.unpostpone(this.task.id)
      .then(function (unpostponed) {
        self.task.going = false;
        self.task.task = unpostponed.task;
      }, onError);
  };

  $scope.update = function () {
    Tasks.update(this.task)
      // TODO don't noop, apply updated task
      .then(undefined, onError);
  };

  $scope.delete = function () {
    this.task.going = true;

    Tasks.delete(this.task.id)
      .then(removeTask, onError);
  };

  $scope.pause = function () {
    this.task.going = true;

    var self = this;
    Tasks.pause(this.task.id)
      .then(function (paused) {
        self.task.going = false;
        self.task.task = paused.task;
      }, onError);
  };

  $scope.unpause = function () {
    this.task.going = true;

    var self = this;
    Tasks.unpause(this.task.id)
      .then(function (unpaused) {
        self.task.going = false;
        self.task.task = unpaused.task;
      }, onError);
  };

  $scope.addEstimate = function (pomos) {
    Tasks.addEstimate(this.task, pomos)
      .then(undefined, onError);
  };

  $scope.logPomo = function () {
    Tasks.logPomos(this.task, 1)
      .then(undefined, onError);
  };
});
