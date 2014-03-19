/*global angular */

angular.module('app.controllers')
.controller('PlansCtrl', function ($scope, initialPlanData, $http, $log) {
  'use strict';

  $scope.plans = initialPlanData;

  $scope.addNewPlan = function () {
    var plan = {id: '_new', plan: this.newPlan};
    this.newPlan = {};
    this.plans.splice(0, 0, plan);
    $http.post("/plans", plan.plan)
      .success(function (data, status, headers, config) {
        plan.id = data.id;
      }).error(function (data, status, headers, config) {
        plan.broken = true;
      });
  };

  $scope.removePlan = function (plan) {
    var index = this.plans.indexOf(plan);
    if (index > -1) {
      this.plans.splice(index, 1);
    }
  };

  $scope.completePlan = function (plan) {
    plan.going = true;
    var self = this;

    $http.post("/plans/" + plan.id + "/complete")
      .success(function (data, status, headers, config) {
        self.removePlan(plan);
      });
  };

  $scope.deletePlan = function (plan) {
    plan.going = true;
    var self = this;
    $http.delete("/plans/" + plan.id)
      .success(function (data, status, headers, config) {
        self.removePlan(plan);
      });
  };

  $scope.updatePlan = function (plan) {
    return $http.put("/plans/" + plan.id, plan.plan)
      .error(function (data, status, headers, config) {
        $log.warn("Failed to update plan:", arguments);
      });
  };
});
