'use strict';

describe('TasksCtrl', function () {
  var scope, ctrl, $q;

  beforeEach(module('app.controllers'));

  function FakeTask(task) { this.task = task || {}; }
  var FakeTaskRepo;

  beforeEach(module(function ($provide) {
    $provide.factory('Task', function () { return FakeTask; });
    FakeTaskRepo = jasmine.createSpyObj('Tasks', [
      'all',
      'create'
    ]);
    FakeTaskRepo.all.and.returnValue([1, 2, 3, 4].map(function (id) {
      var task = {
        id: id,
        task: {
          order: 100 - id
        }
      };
      return task;
    }));
    $provide.factory('Tasks', function () { return FakeTaskRepo; });
  }));

  beforeEach(inject(function($rootScope, $controller, _$q_) {
    scope = $rootScope.$new();
    ctrl = $controller('TasksCtrl', {$scope: scope});
    $q = _$q_;
  }));

  it('should expose an array of tasks', function () {
    expect(isArray(scope.tasks)).toBeTruthy();
  });

  it('should provide a blank task', function () {
    expect(scope.newTask).toBeDefined();
  });

  describe('.addNewTask()', function () {
    beforeEach(function () {
      FakeTaskRepo.create.and.returnValue($q.when({
        id: 42,
        task: {
          title: 'Clean the kitchen'
        }
      }));

      scope.newTask = {task: {title: 'Clean the kitchen', schedule: 'Once'}};
    });

    it('should reset the blank task', function () {
      scope.addNewTask();

      expect(scope.newTask.task.title).toBeUndefined();
    });

    it('should add the task to the array', function () {
      var numTasks = scope.tasks.length;

      scope.addNewTask();

      expect(scope.tasks.length).toBe(numTasks + 1, 'no task was added');
      expect(scope.tasks.filter(function (task) {
        return task.task.title === 'Clean the kitchen';
      }).length).toBe(1, 'no task with correct title');
    });

    it('should save the task', function () {
      scope.addNewTask();

      expect(FakeTaskRepo.create).toHaveBeenCalledWith(jasmine.objectContaining({
        title: 'Clean the kitchen'
      }));
    });

    it('should give the task its server-assigned id', function () {
      var addedTask = scope.addNewTask();

      scope.$apply();
      expect(addedTask.id).toBe(42);
    });

    describe('if adding the task failed', function () {
      beforeEach(function () {
        FakeTaskRepo.create.and.returnValue($q.reject());
      });

      it('should mark the task as broken', function () {
        var addedTask = scope.addNewTask();

        scope.$apply();
        expect(addedTask.broken).toBeTruthy();
      });
    });
  });
});
