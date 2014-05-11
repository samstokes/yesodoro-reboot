'use strict';

describe('TasksCtrl', function () {
  var scope, ctrl, $q;

  beforeEach(module('app.controllers'));

  var fakeMessageFromServer = 'Hello from the server!';
  function FakeTask(task) { this.task = task || {}; }
  var FakeTaskRepo;
  var FakeBillboard;

  beforeEach(module(function ($provide) {
    $provide.value('messageFromServer', fakeMessageFromServer);

    FakeBillboard = jasmine.createSpyObj('Billboard', [
      'success',
      'error'
    ]);
    $provide.value('Billboard', FakeBillboard);

    $provide.factory('Task', function () { return FakeTask; });

    FakeTaskRepo = jasmine.createSpyObj('Tasks', [
      'all',
      'create',
      'complete'
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

  it('should show the message from the server', function () {
    expect(FakeBillboard.success)
      .toHaveBeenCalledWith('Hello from the server!', jasmine.objectContaining({}));
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

      it('should add the task to the array', function () {
        expect(scope.tasks).toContain(jasmine.objectContaining({id: 42}));
        expect(scope.tasks).not.toContain(jasmine.objectContaining({id: '_new'}));
      });
    });

    describe('if adding the task failed', function () {
      beforeEach(function () {
        FakeTaskRepo.create.and.returnValue($q.reject());
        scope.addNewTask();
        scope.$apply();
      });

      it('should not add the task to the array', function () {
        expect(scope.tasks).not.toContain(jasmine.objectContaining({id: '_new'}));
      });

      it('should refill the blank task with the task title', function () {

        expect(scope.newTask.task.title).toBe('Clean the kitchen');
      });
    });
  });


  describe('completing a task', function () {
    var task;
    var completedTask = {
      id: 42,
      task: {
        title: 'Clean the kitchen',
        done_at: 'JUST NOW'
      }
    };

    beforeEach(function () {
      FakeTaskRepo.complete.and.returnValue($q.when({
        completed: completedTask
      }));

      task = {id: 42, task: {}};
    });

    it('should mark the task as going away', function () {
      scope.completeTask(task);

      expect(task.going).toBeTruthy();
    });

    it('should sync the completion', function () {
      scope.completeTask(task);

      expect(FakeTaskRepo.complete).toHaveBeenCalledWith(42);
    });

    describe('if the sync succeeds', function () {
      beforeEach(function () {
        scope.completeTask(task);
        scope.$apply();
      });

      it('should mark the task as done', function () {
        expect(task.task.done_at).toBe('JUST NOW');
      });

      it('should clear the "going away" flag (since the task will actually go now)', function () {
        expect(task.going).toBeFalsy();
      });
    });

    describe('if the task recurs', function () {
      beforeEach(function () {
        FakeTaskRepo.complete.and.returnValue($q.when({
          completed: completedTask,
          recurred: {
            id: 43,
            task: {title: 'Clean the kitchen'}
          }
        }));

        scope.completeTask(task);
        scope.$apply();
      });

      it('should add the recurred task to the array', function () {
        expect(scope.tasks).toContain(jasmine.objectContaining({id: 43}));
      });
    });
  });
});
