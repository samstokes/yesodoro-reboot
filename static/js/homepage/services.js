/*global angular */
angular.module('homepage.services', ['app.models'])
.factory('Tasks', function ($q, Task) {
  var Tasks = {};

  Tasks.create = function (task) {
    return $q.when(new Task({
      task: task
    }));
  };

  Tasks.complete = function (id) {
    return $q.when({completed: {
      task: {}
    }});
  };

  Tasks.postpone = function (id) {
    return $q.when({
      task: {}
    });
  };

  function noop() {
    return $q.when();
  }

  Tasks.delete = noop;
  Tasks.reorder = noop;

  return Tasks;
})
.factory('tasks', function (Task) {
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
