'use strict';

describe('NotesCtrl', function () {
  var scope, ctrl, task;

  beforeEach(module('app.controllers'));

  beforeEach(inject(function($rootScope, $controller) {
    task = {id: 42, notes: []};

    scope = $rootScope.$new();
    scope.task = task;
    ctrl = $controller('NotesCtrl', {$scope: scope});
  }));

  it('should provide a blank note', function () {
    expect(scope.newNote).toBeDefined();
  });

  describe('adding a note', function () {
    var $httpBackend;

    beforeEach(inject(function (_$httpBackend_) {
      $httpBackend = _$httpBackend_;
      $httpBackend.whenPOST('/tasks/42/notes').
        respond({id: 139, note: {body: 'Follow up on Monday'}});

      scope.newNote = {body: 'Follow up on Monday'};
    }));

    afterEach(function () {
      $httpBackend.verifyNoOutstandingExpectation();
    });

    it('should reset the blank note', function () {
      scope.addNewNote();

      expect(scope.newNote.body).toBeUndefined();
    });

    it('should add the note to the task', function () {
      scope.addNewNote();

      expect(task.notes.length).toBe(1);
      expect(task.notes[0].note.body).toEqual('Follow up on Monday');
    });

    it('should store the note on the server', function () {
      $httpBackend.expectPOST('/tasks/42/notes');

      scope.addNewNote();
    });

    it('should give the note its server-assigned id', function () {
      var addedNote = scope.addNewNote();
      $httpBackend.flush();

      expect(addedNote.id).toBe(139);
    });

    describe('if adding the note failed', function () {
      beforeEach(function () {
        // TODO this only works because 'expect' precedes 'when'...
        // this should really be a 'when', but successive whens don't override
        $httpBackend.expectPOST('/tasks/42/notes').
          respond(401, 'bad robot');
      });

      it('should mark the note as broken', function () {
        var addedNote = scope.addNewNote();
        $httpBackend.flush();

        expect(addedNote.broken).toBeTruthy();
      });
    });
  });
});
