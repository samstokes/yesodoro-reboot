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

  describe('adding a task', function () {
    beforeEach(function () {
      FakeTaskRepo.create.and.returnValue($q.when({
        id: 42,
        task: {
          title: 'Clean the kitchen'
        }
      }));

      scope.newTask = {id: '_new', task: {title: 'Clean the kitchen', schedule: 'Once'}};
    });

    it('should reset the blank task', function () {
      scope.addNewTask();

      expect(scope.newTask.task.title).toBeUndefined();
    });

    it('should add the task to the array', function () {
      scope.addNewTask();

      expect(scope.tasks).toContain(jasmine.objectContaining({id: '_new'}));
    });

    it('should save the task', function () {
      scope.addNewTask();

      expect(FakeTaskRepo.create).toHaveBeenCalledWith(jasmine.objectContaining({
        title: 'Clean the kitchen'
      }));
    });

    describe('if adding the task succeeded', function () {
      var addedTask;

      beforeEach(function () {
        addedTask = scope.addNewTask();
        scope.$apply();
      });

      it('should give the task its server-assigned id', function () {
        expect(addedTask.id).toBe(42);
      });

      it('should update the task in the array', function () {
        expect(scope.tasks).toContain(jasmine.objectContaining({id: 42}));
        expect(scope.tasks).not.toContain(jasmine.objectContaining({id: '_new'}));
      });
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
