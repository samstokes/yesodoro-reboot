'use strict';

describe('Tasks', function () {
  var Tasks, $httpBackend, $rootScope;

  beforeEach(module('app.services'));

  function FakeTask() {}

  beforeEach(module(function ($provide) {
    $provide.factory('Task', function () { return FakeTask; });
  }));

  beforeEach(inject(function(_Tasks_, _$httpBackend_, _$rootScope_) {
    Tasks = _Tasks_;
    $httpBackend = _$httpBackend_;
    $rootScope = _$rootScope_;
  }));

  afterEach(function() {
    $httpBackend.verifyNoOutstandingExpectation();
  });

  describe('.all()', function () {
    beforeEach(function () {
      $httpBackend.whenGET('/tasks_json').respond([
        'foo', 'bar', 'baz'
      ]);
    });

    it('should request the tasks from the server', function () {
      $httpBackend.expectGET('/tasks_json');
      Tasks.all();
    });

    it('should return a promise of an array of tasks', function () {
      var allTasks = Tasks.all();
      expect(allTasks).toBeAPromise();

      var tasks;
      allTasks.then(function (_tasks) { tasks = _tasks; });
      $httpBackend.flush();

      expect(isArray(tasks)).toBeTruthy();
    });

    it('should construct Task objects from the task data', function () {
      var tasks;
      Tasks.all().then(function (_tasks) { tasks = _tasks; });
      $httpBackend.flush();

      var tasksThatAreNotTasks = tasks.filter(function (task) {
        return task.constructor !== FakeTask;
      });
      expect(tasksThatAreNotTasks).toBeEmpty();
    });
  });

  describe('.today()', function () {
    beforeEach(function () {
      $httpBackend.whenGET('/tasks_json?q=today').respond([
        'foo', 'bar', 'baz'
      ]);
    });

    it('should request the tasks from the server', function () {
      $httpBackend.expectGET('/tasks_json?q=today');
      Tasks.today();
    });

    it('should return a promise of an array of tasks', function () {
      var todayTasks = Tasks.today();
      expect(todayTasks).toBeAPromise();

      var tasks;
      todayTasks.then(function (_tasks) { tasks = _tasks; });
      $httpBackend.flush();

      expect(isArray(tasks)).toBeTruthy();
    });

    it('should construct Task objects from the task data', function () {
      var tasks;
      Tasks.today().then(function (_tasks) { tasks = _tasks; });
      $httpBackend.flush();

      var tasksThatAreNotTasks = tasks.filter(function (task) {
        return task.constructor !== FakeTask;
      });
      expect(tasksThatAreNotTasks).toBeEmpty();
    });
  });

  describe('.later()', function () {
    beforeEach(function () {
      $httpBackend.whenGET('/tasks_json?q=postponed').respond([
        'foo', 'bar', 'baz'
      ]);
    });

    it('should request the tasks from the server', function () {
      $httpBackend.expectGET('/tasks_json?q=postponed');
      Tasks.later();
    });
  });

  describe('.done()', function () {
    beforeEach(function () {
      $httpBackend.whenGET('/tasks_json?q=done').respond([
        'foo', 'bar', 'baz'
      ]);
    });

    it('should request the tasks from the server', function () {
      $httpBackend.expectGET('/tasks_json?q=done');
      Tasks.done();
    });
  });

  describe('done(days)', function () {
    beforeEach(function () {
      $httpBackend.whenGET(/\/tasks_json\?.*q=done/).respond([
        'foo', 'bar', 'baz'
      ]);
    });

    it('should request the tasks from the server', function () {
      $httpBackend.expectGET(/q=done&days=42|days=42&q=done/);
      Tasks.done(42);
    });

    it('should error if days is a string', function () {
      var error;
      Tasks.done('42').then(null, function (_error) { error = _error; });

      $httpBackend.verifyNoOutstandingRequest();

      $rootScope.$digest();
      expect(error).toBeDefined();
      expect(error).toMatch(/days/);
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
