import * as chai from 'chai';
import * as sinon from 'sinon';

import {applyMicroservicesRoutes} from '../src/microservice';

chai.use(require('sinon-chai'));

chai.use(require('chai-diff'));

const {expect} = chai;

describe('microservice', function (): void {
    it('Applies Routes and middlewares',(): void => {
        const root = sinon.spy();
        const getSpy = sinon.spy();
        const useSpy = sinon.spy();
        const enableSpy = sinon.spy();
        const appMock = {
            'get': getSpy,
            'use': useSpy,
            'enable': enableSpy
        };
        const routesDefinition = {
            'GET': {
                '/': [root]
            }
        };
        const middlewares = ['a','b'];
        applyMicroservicesRoutes(routesDefinition,middlewares)(appMock);
        // @ts-ignore
        expect(useSpy).to.have.been.calledWith('a');

        // @ts-ignore
        expect(getSpy).to.have.been.calledWith('/',root);
    });
});
