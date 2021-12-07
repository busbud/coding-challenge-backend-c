import { Request, Response } from 'express';
import { suggestionSchema } from '../suggestion-schema';

const mockedNext = jest.fn();
const mockedRes = {};

describe('suggestionSchema', () => {
  it('should validate only term without errors', () => {
    const req = { query: { q: '_term_' } };

    suggestionSchema(req as unknown as Request, mockedRes as unknown as Response, mockedNext);
    expect(mockedNext).toBeCalledWith();
  });

  it('should validate term and location without errors', () => {
    const req = { query: { q: '_term_', lat: 1, long: 2 } };

    suggestionSchema(req as unknown as Request, mockedRes as unknown as Response, mockedNext);
    expect(mockedNext).toBeCalledWith();
  });

  it('should validate with error - missing long peer', () => {
    const req = { query: { q: '_term_', lat: 1 } };

    const expected = { "message": "Validation error: \"value\" contains [lat] without its required peers [long]", "statusCode": 400 };
    suggestionSchema(req as unknown as Request, mockedRes as unknown as Response, mockedNext);
    expect(mockedNext).toBeCalledWith(expected);
  });

  it('should validate with error - missing lat peer', () => {
    const req = { query: { q: '_term_', long: 1 } };

    const expected = { "message": "Validation error: \"value\" contains [long] without its required peers [lat]", "statusCode": 400 };
    suggestionSchema(req as unknown as Request, mockedRes as unknown as Response, mockedNext);
    expect(mockedNext).toBeCalledWith(expected);
  });
});
