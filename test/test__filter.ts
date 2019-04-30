// import * as chai from 'chai';

// import filter from '../src/filter';
// import fixtures from './fixtures';

// chai.use(require('chai-diff'));

// const {expect} = chai;


// /**
//  * @summary Builds the given test from the fixtures object
//  * @param name {String}
//  */
// const buildExpectFromFixtures = (fixtures: any): Function => (name: string): void => {
//     const fixture = fixtures[name];
//     if (!fixture) {
//         throw new Error('Fixture not found in fixtures.ts');
//     }
//     const actual = filter(fixture.settings,fixture.components);
//     // @ts-ignore (https://github.com/chaijs/chai/issues/1100)
//     expect(actual).not.differentFrom(fixture.expected);
// };

// // Uses the builder for this test fixtures from './fixtures.ts'
// const assertFromFixtures: Function = buildExpectFromFixtures(fixtures);

// describe('filterService', function (): void {

//     it('Allows changing default option', (): void => {
//         const fixture = fixtures.oneRequire;
//         const actual = filter(fixture.settings,fixture.components,{omitRequires: false});
//         // @ts-ignore (https://github.com/chaijs/chai/issues/1100)
//         expect(actual).not.differentFrom(fixture.settings);
//     });

//     it('Handles empty arguments',(): void => {
//         assertFromFixtures('empty');
//     });

//     it('Handles empty settings',(): void => {
//         assertFromFixtures('settingsEmpty');
//     });

//     it('Handles empty components',(): void => {
//         assertFromFixtures('componentsEmpty');
//     });

//     it('Handles mixedNoRequire',(): void => {
//         assertFromFixtures('mixedNoRequire');
//     });

//     it('Handles one result from components',(): void => {
//         assertFromFixtures('mixedNoRequire');
//     });

//     it('Handles two results in one setting',(): void => {
//         assertFromFixtures('mixedTwoResultsOneSetting');
//     });

//     it('Handles two results in two settings',(): void => {
//         assertFromFixtures('mixedTwoResultsTwoSettings');
//     });

//     it('Handles three settings and no results',(): void => {
//         assertFromFixtures('sampleTestCase');
//     });

//     it('Handles presentation test case',(): void => {
//         assertFromFixtures('enhancedTestCase');
//     });
// });
