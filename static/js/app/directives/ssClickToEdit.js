'use strict';

angular.module('app.directives')
.directive('ssClickToEdit', function ($timeout) {
  return {
    restrict: 'E',

    scope: {
      value: '=ssModel',
      onEdit: '&ssOnEdit'
    },

    transclude: true,


    template: ' \
      <div ng-hide="editing" ng-click="switchToEdit()" ng-transclude /> \
      <form ng-submit="save()"> \
        <input class="editor" \
            ng-model="value" \
            ng-show="editing" \
            ng-blur="editorBlur()" \
            ss-escape="leaveEdit(true)" \
        /> \
    ',


    link: function (scope, element, attrs) {

      scope.editing = false;

      scope.switchToEdit = function () {
        var editor = element.find('input.editor');
        $timeout(function () { editor.focus(); });
        this.original = this.value;
        this.editing = true;
      };

      scope.leaveEdit = function (discard) {
        this.editing = false;

        if (discard) {
          this.value = this.original;
        }
      };

      scope.save = function () {
        var onEdit = this.onEdit();
        if ("undefined" === typeof(onEdit)) {
          return this.leaveEdit(false);
        }
        onEdit.then(
          angular.bind(this, this.leaveEdit, false),
          angular.bind(this, this.leaveEdit, true));
      };

      scope.editorBlur = function () {
        if (this.editing) {
          this.save();
        }
      };

    }
  };
});
