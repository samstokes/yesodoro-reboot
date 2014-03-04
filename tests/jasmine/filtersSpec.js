'use strict';

describe('app.filters', function () {
  beforeEach(module('app.filters'));

  describe('groupBy', function () {
    var groupByFilter;

    beforeEach(inject(function (_groupByFilter_) {
      groupByFilter = _groupByFilter_;
    }));

    function personNamed(name) {
      return jasmine.objectContaining({name: name});
    }


    it('should return empty for an empty input', function () {
      expect(groupByFilter([], 'homeTown').length).toBe(0);
    });

    describe('grouping people by home town', function () {
      var people = [
        {name: 'Bob', homeTown: 'New York City'},
        {name: 'Sam', homeTown: 'London'},
        {name: 'Alice', homeTown: 'New York City'},
        {name: 'Alex', homeTown: 'London'},
        {name: 'Dave', homeTown: 'San Francisco'}
      ];

      function town(name) {
        return jasmine.objectContaining({homeTown: name});
      }


      var filtered;
      beforeEach(function () {
        filtered = groupByFilter(people, 'homeTown');
      });

      function townPeople(town) {
        var townAndPeople = filtered.filter(function (_townAndPeople) {
          return _townAndPeople.homeTown === town;
        })[0];
        if (townAndPeople === undefined) return undefined;
        return townAndPeople.items;
      }


      it('should return an array of group objects', function () {
        expect(isArray(filtered)).toBeTruthy('filtered is not an array!');
        filtered.forEach(function (group) {
          expect(group.homeTown).toBeDefined();
          expect(isArray(group.items)).toBeTruthy('group ' + group.homeTown + ' items is not an array!');
        });
      });

      it('should return a group for each home town', function () {
        expect(filtered).toContain(town('New York City'));
        expect(filtered).toContain(town('London'));
        expect(filtered).toContain(town('San Francisco'));
      });

      it('should group the people by home town', function () {
        expect(townPeople('New York City')).toContain(personNamed('Alice'));
        expect(townPeople('New York City')).toContain(personNamed('Bob'));
        expect(townPeople('London')).toContain(personNamed('Sam'));
        expect(townPeople('London')).toContain(personNamed('Alex'));
        expect(townPeople('San Francisco')).toContain(personNamed('Dave'));
      });

      it('should include no empty groups', function () {
        expect(filtered).not.toContain(jasmine.objectContaining({items: []}));
      });

      it('should preserve order of items within groups', function () {
        function names(people) {
          return people.map(function (person) { return person.name; });
        }
        expect(names(townPeople('New York City'))).toEqual(['Bob', 'Alice']);
        expect(names(townPeople('London'))).toEqual(['Sam', 'Alex']);
      });

      it('should return groups in order of first seen (so pre-sorting works)', function () {
        var towns = filtered.map(function (group) {
          return group.homeTown;
        });

        expect(towns).toEqual(['New York City', 'London', 'San Francisco']);
      });
    });

    it('should work with object methods too', function () {
      function Person(data) {
        mergeInto(this, data);
      }
      Person.prototype.uptown = function () {
        return this.homeTown.toUpperCase();
      };
      var people = [
        {name: 'Bob', homeTown: 'New York City'},
        {name: 'Sam', homeTown: 'London'},
        {name: 'Alice', homeTown: 'New York City'},
        {name: 'Alex', homeTown: 'London'},
        {name: 'Dave', homeTown: 'San Francisco'}
      ].map(function (data) { return new Person(data); });

      function town(name) {
        return jasmine.objectContaining({uptown: name});
      }

      var filtered = groupByFilter(people, 'uptown');

      expect(filtered).toContain(town('LONDON'));
      var LONDONers = filtered.filter(function (townsAndPeople) {
        return townsAndPeople.uptown === 'LONDON';
      })[0];
      expect(LONDONers.items).toContain(personNamed('Sam'));
      expect(LONDONers.items).toContain(personNamed('Alex'));
    });
  });
});
