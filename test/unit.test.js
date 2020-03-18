var expect  = require('chai').expect;
var app     = require('../app');
var request = require('supertest')(app);

import { getCitiesThatMatchName } from '../src/utils/functions';

const testData = [
    { "name": "Abbotsford", "lat": "49.05798", "long": "-122.25257" },
    { "name": "Acton Vale", "ascii": "Acton Vale", "alt_name": "", "lat": "45.65007", "long": "-72.56582" },
    { "name": "Airdrie", "lat": "51.30011", "long": "-114.03528" },
    { "name": "Ajax", "ascii": "Ajax", "alt_name": "Adzhaks,Ehjdzhaks,Ejdzaks,ajaks, awntaryw,eijegseu,Аджакс,Ејџакс,Эйджакс,أجاكس، أونتاريو,에이젝스", "lat": "43.85012", "long": "-79.03288" },
    { "name": "Alma", "ascii": "Alma", "alt_name": "Al'ma,Alma,YTF,alma,alma, kbk,alma, kybk,Алма,Альма,آلما,آلما، کبک,ألما، كيبك", "lat": "48.55009", "long": "-71.6491" },
    { "name": "Amherstburg", "ascii": "Amherstburg", "alt_name": "", "lat": "42.11679", "long": "-83.04985" },
    { "name": "Amos", "ascii": "Amos", "alt_name": "Amos,YEY,amws,amws, kbk,amws, kybk,Амос,آموس، کبک,أموس، كيبك,اموس", "lat": "48.56688", "long": "-78.11624" },
    { "name": "Angus", "ascii": "Angus", "alt_name": "", "lat": "44.31681", "long": "-79.88295" },
    { "name": "Anmore", "ascii": "Anmore", "alt_name": "", "lat": "49.31637", "long": "-122.85263" },
    { "name": "Antigonish", "ascii": "Antigonish", "alt_name": "", "lat": "45.61685", "long": "-61.99858" }
];

describe('getCitiesThatMatchName', () => {
    it('returns empty array for no match', () => {
        const lookUpCity = 'THERE IS NOT CITY';
        const filteredCities = getCitiesThatMatchName(lookUpCity, testData);
        expect(filteredCities).to.be.instanceof(Array);
        expect(filteredCities).to.have.length(0);
    }),

    it('filters for one matching city', () => {
        const lookUpCity = 'Ajax';
        const filteredCities = getCitiesThatMatchName(lookUpCity, testData);
        expect(filteredCities).to.be.instanceof(Array);
        expect(filteredCities).to.have.length(1);
        // expect(filteredCities).to.include([{name:'Ajax'}]);
        
    });

    it('filters for matching city names', () => {
        const lookUpCity = 'An';
        const filteredCities = getCitiesThatMatchName(lookUpCity, testData);
        expect(filteredCities).to.be.instanceof(Array);
        expect(filteredCities).to.have.length(3);
    });
});

