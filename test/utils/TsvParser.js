import path from 'path';
import { expect } from 'chai';

import { fixturePath } from '../test_setup';

import { TsvParser } from '../../src/utils';

const testFilePath = path.join(fixturePath, 'ts_parser_test_data.tsv');

const expectObservableValues = [
  {
    id: '6058560',
    name: 'London',
    lat: '42.98339',
    long: '-81.23304',
    population: '346765',
  },
  {
    id: '6059891',
    name: 'Longueuil',
    lat: '45.53121',
    long: '-73.51806',
    population: '229330',
  },
  {
    id: '6060407',
    name: 'Lorraine',
    lat: '45.68338',
    long: '-73.78249',
    population: '9613',
  },
];

describe('TsvParser', () => {
  let tsvParser;

  beforeEach(() => {
    tsvParser = new TsvParser({ filePath: testFilePath });
  });

  it('#parse', (done) => {
    const values = [];
    tsvParser.parse().subscribe({
        next: (row) => values.push(row),
        complete: () => {
          expect(values).to.deep.equal(expectObservableValues);
          done();
        },
      },
    );
  });
});

