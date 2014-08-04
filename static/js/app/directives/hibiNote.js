/*global angular */

angular.module('app.directives')
.directive('hibiNote', function () {
  'use strict';

  return {
    restrict: 'A',
    scope: {
      note: '=hibiNote'
    },
    template:
      '<div class="note">' +
        '<markdown class="body" ng-model="note.note.body" />' +
        '<div class="created">' +
          '{{note.note.createdAt | date:dateFormat}}' +
        '</div>' +
      '</div>'
  };
});
