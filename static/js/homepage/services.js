/*global angular */
angular.module('homepage.services', ['app.models'])
.factory('Tasks', function ($timeout, Task) {
  'use strict';

  var Tasks = {};

  Tasks.create = function (task) {
    return soon(function () {
      return new Task({task: task});
    });
  };

  Tasks.complete = function (id) {
    return soon(function () {
      return {completed: {task: {}}};
    });
  };

  Tasks.postpone = function (id) {
    return soon(function () {
      return {task: {}};
    });
  };

  function soon(f) {
    return $timeout(f, 200);
  }

  function noop() {
    return soon(function () {});
  }

  Tasks.delete = noop;
  Tasks.reorder = noop;

  return Tasks;
})
.factory('tasks', function (Task) {
  'use strict';

  return [
    'Hello',
    'world'
  ].map(function (title) {
    return {task: {
      title: title,
      schedule: 'Once',
      active: true,
      scheduled_for: new Date()
    }};
  }).map(function (task) { return new Task(task); });
});
