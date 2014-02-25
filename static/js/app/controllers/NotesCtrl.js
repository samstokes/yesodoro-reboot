'use strict';

angular.module('app.controllers')
.controller('NotesCtrl', function ($scope, $http) {
  $scope.notes = $scope.task.notes;
  $scope.newNote = {};

  $scope.addNewNote = function () {
    var note = {id: '_new', note: this.newNote};
    this.newNote = {};
    this.notes.splice(this.notes.length, 0, note);
    $http.post("/tasks/" + this.task.id + '/notes', note.note)
      .success(function (data, status, headers, config) {
        note.id = data.id;
        note.note = data.note;
      }).error(function (data, status, headers, config) {
        note.broken = true;
      });
  };
});
