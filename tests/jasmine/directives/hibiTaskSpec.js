'use strict';

describe('hibiTask controller', function () {
  beforeEach(module('app.directives'));

  var FakeTaskRepo;

  beforeEach(module(function ($provide) {
    FakeTaskRepo = jasmine.createSpyObj('Tasks', [
      'complete'
    ]);
    $provide.factory('Tasks', function () { return FakeTaskRepo; });
  }));

  var $q,
      Tasks,
      task,
      scope;

  beforeEach(inject(function (_$q_, $rootScope, _Tasks_, $controller) {
    task = {id: 42, task: {}};

    $q = _$q_;
    Tasks = _Tasks_;

    scope = $rootScope.$new();
    scope.task = task;

    [ 'onExtraTask'
    , 'onComplete'
    ].forEach(function (callback) {
      scope[callback] = jasmine.createSpy(callback);
    });

    $controller('HibiTaskCtrl', {$scope: scope, $element: undefined, $attrs: undefined});
  }));


  describe('completing a task', function () {
    var completedTask = {
      id: 42,
      task: {
        title: 'Clean the kitchen',
        done_at: 'JUST NOW'
      }
    };

    beforeEach(function() {
      FakeTaskRepo.complete.and.returnValue($q.when({
        completed: completedTask
      }));
    });

    it('should mark the task as going away', function () {
      scope.complete();

      expect(task.going).toBeTruthy();
    });

    it('should sync the completion', function () {
      scope.complete();

      expect(FakeTaskRepo.complete).toHaveBeenCalledWith(42);
    });

    describe('if the sync succeeds', function () {
      beforeEach(function () {
        scope.complete();
        scope.$apply();
      });

      it('should mark the task as done', function () {
        expect(task.task.done_at).toBe('JUST NOW');
      });

      it('should clear the "going away" flag (since the task will actually go now)', function () {
        expect(task.going).toBeFalsy();
      });

      it('should call the onComplete callback', function () {
        expect(scope.onComplete).toHaveBeenCalled();
      });
    });

    describe('if the task recurs', function () {
      var recurredTask;

      beforeEach(function () {
        recurredTask = {task: {dummy: 'task'}};

        FakeTaskRepo.complete.and.returnValue($q.when({
          completed: completedTask,
          recurred: recurredTask
        }));

        scope.complete();
        scope.$apply();
      });

      it('should pass the recurred task to the onExtraTask callback', function () {
        expect(scope.onExtraTask).toHaveBeenCalledWith({extraTask: recurredTask});
      });
    });
  });
});
