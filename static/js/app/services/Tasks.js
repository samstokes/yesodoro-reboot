/*global
   angular
 , mergeInto
 */

angular.module('app.services')
.factory('Tasks', function (Task, $q, $http) {
  'use strict';

  function newTask(taskData) {
    return new Task(taskData);
  }

  function extractData(response) {
    return response.data;
  }

  function taskAction(action) {
    return function (taskId) {
      return $http.post("/tasks/" + taskId + "/" + action)
        .then(extractData)
        .then(newTask);
    };
  }

  var Tasks = {};

  Tasks.all = function () {
    return $http.get('/tasks_json')
      .then(extractData)
      .then(function (taskData) {
        return taskData.map(newTask);
      });
  };

  Tasks.create = function (task) {
    return $http.post("/tasks", task)
      .then(extractData)
      .then(newTask);
  };

  Tasks.delete = function (taskId) {
    return $http.delete("/tasks/" + taskId)
      .then(extractData);
  };

  Tasks.complete = function (taskId) {
    return $http.post("/tasks/" + taskId + "/complete")
      .then(extractData)
      .then(function (data) {
        var tasks = {};
        if (data.completed) {
          tasks.completed = new Task(data.completed);
        }
        if (data.recurred) {
          tasks.recurred = new Task(data.recurred);
        }
        return tasks;
      });
  };

  Tasks.update = function (task) {
    return $http.put("/tasks/" + task.id, task.task)
      .then(extractData)
      .then(newTask);
  };

  Tasks.edit = function (task, editType, editOptions) {
    var edit = mergeInto({editType: editType}, editOptions);
    return $http({method: 'PATCH', url: '/tasks/' + task.id,
      data: edit
    })
      .then(extractData)
      .then(newTask)
      .then(function (editedTask) {
        task.task = editedTask.task;
        return editedTask;
      });
  };

  Tasks.reorder = function (task, delta) {
    return Tasks.edit(task, 'order', {delta: delta});
  };

  Tasks.logPomos = function (task, pomos) {
    task.task.pomos += pomos;
    return Tasks.edit(task, 'pomos', {pomos: pomos})
      .then(undefined, function (reason) {
        task.task.pomos -= pomos;
        return $q.reject(reason);
      });
  };

  Tasks.addEstimate = function (task, pomos) {
    var estimate = task.newEstimate(pomos);
    var index = task.addEstimate(estimate);
    return $http.post("/tasks/" + task.id + "/estimates", estimate.estimate)
      .then(extractData, function (reason) {
        task.removeEstimate(index);
        return $q.reject(reason);
      });
  };

  [
    'restart',
    'postpone', 'unpostpone',
    'pause', 'unpause'
  ].forEach(function (action) {
    Tasks[action] = taskAction(action);
  });

  return Tasks;
});
