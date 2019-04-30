import * as chai from 'chai';
import * as sinon from 'sinon';

import {defaults, logger} from '../src/middlewares/logger';

chai.use(require('sinon-chai'));

chai.use(require('chai-diff'));

const {expect} = chai;

describe('middlewares', function (): void {

    it('Logs using default logger', (): void => {
        sinon.stub(console,'log');

        logger()({
            headers: {},
            connection: {}
        },{},()=>{});
        // @ts-ignore
        expect(console.log).calledOnce;
        // @ts-ignore
        console.log.restore();
    });

    it('Logs using provided logger', (): void => {
        const spyLogger = sinon.spy();
        const nextLogger = sinon.spy();
        const loggerMiddleware = logger({
            loggerFunction: spyLogger
        });
        loggerMiddleware({
            headers: {},
            connection: {}
        },{},nextLogger);
        // debugger;
        // @ts-ignore
        expect(spyLogger.getCall(0).args[1].logLevel).to.equal(4);
        // @ts-ignore
        expect(nextLogger).calledOnce;
    });

    it('Defaults only override unexisting properties',(): void => {
        const received = {
            a: 1,
            b: 2
        };
        const deflt = {
            b: 3,
            c: 4
        };
        // @ts-ignore
        const actual = defaults(received,deflt);
        // @ts-ignore (https://github.com/chaijs/chai/issues/1100)
        expect(actual).not.differentFrom({
            b: 2,
            c: 4,
            a: 1,
        });
    });
});
