'use strict';

describe('Tasks', function () {
  var Tasks, $httpBackend;

  beforeEach(module('app.services'));

  function FakeTask() {}

  beforeEach(module(function ($provide) {
    $provide.factory('Task', function () { return FakeTask; });
    $provide.value('initialTaskData', [1, 2, 3, 4]);
  }));

  beforeEach(inject(function(_Tasks_, _$httpBackend_) {
    Tasks = _Tasks_;
    $httpBackend = _$httpBackend_;
  }));

  afterEach(function() {
    $httpBackend.verifyNoOutstandingExpectation();
  });

  describe('.all()', function () {
    var allTasks;
    beforeEach(function () { allTasks = Tasks.all(); });

    it('should return an array of tasks', function () {
      expect(isArray(allTasks)).toBe(true);
    });

    it('should construct Task objects from the task data', function () {
      var tasksThatAreNotTasks = allTasks.filter(function (task) {
        return task.constructor !== FakeTask;
      });
      expect(tasksThatAreNotTasks).toEqual([]);
    });
  });

  describe('.create(task)', function () {
    var task = {
      title: 'Test the Tasks service'
    };

    beforeEach(function () {
      $httpBackend.whenPOST('/tasks').respond({
        id: 42,
        task: {title: 'Test the Tasks service'}
      });
    });

    it('should save the task to the server', function () {
      $httpBackend.expectPOST('/tasks', task);
      Tasks.create(task);
    });

    it('should return a Task', function () {
      var result;
      Tasks.create(task).then(function (_result) { result = _result; });

      $httpBackend.flush();
      expect(result.constructor).toBe(FakeTask);
    });
  });

  describe('.complete(taskId)', function () {
    it('should tell the server to complete the task', function () {
      $httpBackend.expectPOST('/tasks/42/complete').respond({
        completed: 'yeah'
      });
      Tasks.complete(42);
    });

    describe('if the task was recurring', function () {
      beforeEach(function () {
        $httpBackend.whenPOST('/tasks/43/complete').respond({
          completed: 'completedTask',
          recurred: 'recurredTask'
        });
      });

      it('should return the recurrence as a Task', function () {
        var result;
        Tasks.complete(43).then(function (_result) { result = _result; });

        $httpBackend.flush();
        expect(result.recurred.constructor).toBe(FakeTask);
      });
    });
  });
});
