/* eslint-disable @typescript-eslint/no-explicit-any */
import { CitiesController } from "../citiesController";

import chai from "chai";
const { expect } = chai;
import { Request } from "express";

import sinon from "sinon";

describe("CitiesController", () => {
  const citySuggestions = sinon.spy(CitiesController, "citySuggestions");

  let status: any;
  let json: any;
  let res: any;
  let send: any;
  beforeEach(() => {
    status = sinon.stub();
    json = sinon.spy();
    res = { json, status, send };
    status.returns(res);
  });

  afterEach(function () {
    sinon.restore();
  });

  it(`[Given]: City Autocomplete Suggestion is required
    [WHEN] citySuggestions endpoint is called
    [AND]: City name query param is not passed as q in request query
    [THEN]: Bad request error is returned`, async () => {
    const req = { query: {} } as Request;
    await CitiesController.citySuggestions(req, res);

    expect(status.args[0][0]).to.equal(400);
    expect(json.args[0][0].message).to.include('"cityName" is required');
  });

  it(`[Given]: City Autocomplete Suggestion is required
    [WHEN] citySuggestions endpoint is called
    [AND]: Longitude, City name query param is passed as lon and q respectively in request query
    [AND]: Latitude is not passed
    [THEN]: Bad request error is returned for latitude`, async () => {
    const req = {} as Request;
    req.query = {
      q: "tor",
      lon: "-75",
    };
    await CitiesController.citySuggestions(req, res);

    expect(status.args[0][0]).to.equal(400);
    expect(json.args[0][0].message).to.include("Invalid Query Parameters");
  });

  it(`[Given]: City Autocomplete Suggestion is required
    [WHEN] citySuggestions endpoint is called
    [AND]: Latitude, City name query param is passed as lon and q respectively in request query
    [AND]: Longitude is not passed
    [THEN]: Bad request error is returned for longitude`, async () => {
    const req = {} as Request;
    req.query = {
      q: "tor",
      lon: "-75",
    };
    await CitiesController.citySuggestions(req, res);

    expect(status.args[0][0]).to.equal(400);
    expect(json.args[0][0].message).to.include("Invalid Query Parameters");
  });

  it(`[Given]: City Autocomplete Suggestion is required
      [WHEN] citySuggestions endpoint is called
      [AND]: City name query param is passed as q in request query
      [THEN]: List of cities based on search query city name is returned`, async () => {
    const req = {} as Request;
    req.query = {
      q: "tor",
    };

    await CitiesController.citySuggestions(req, res);

    expect(status.args[0][0]).to.equal(200);
    expect(json.args[0][0].suggestions?.length).to.be.greaterThan(0);
    expect(citySuggestions.called).to.be.true;
  });

  it(`[Given]: City Autocomplete Suggestion is required
      [WHEN] citySuggestions endpoint is called
      [AND]: Invalid city name query param is passed as q in request query
      [THEN]: List of cities based on search query city name is returned`, async () => {
    const req = {} as Request;
    req.query = {
      q: "cannotBeFound",
    };

    await CitiesController.citySuggestions(req, res);

    expect(status.args[0][0]).to.equal(404);
    expect(json.args[0][0].suggestions?.length).to.equal(0);
    expect(citySuggestions.called).to.be.true;
  });
});
