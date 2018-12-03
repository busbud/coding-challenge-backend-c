const { get, set, clear, fromCacheOr } = require("../utils/cache");

describe("Cache", () => {
  beforeAll(() => {
    clear();
  });
  it("should set a value using a custom seriaizer", () => {
    set("key", { a: 1 });
    expect(get("key")).toEqual({ json: { a: 1 }, serialized: '{"a":1}' });
    set("key2", { a: 2 }, value => value.a * 2);
    expect(get("key2")).toEqual({ json: 4, serialized: 4 });
    clear();
    expect(get("key")).toMatchSnapshot();
  });

  it("should cache a given operation or execute it the first time", () => {
    const op = jest.fn(() => ({ a: 1 }));
    fromCacheOr("key", op);
    expect(op).toBeCalledTimes(1);
    const result = fromCacheOr("key", op);
    expect(op).toBeCalledTimes(1);
    expect(result).toMatchSnapshot();
  });
});
