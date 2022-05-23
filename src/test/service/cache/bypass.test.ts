import BypassCache from 'service/cache/bypass';

let cache: BypassCache;

describe('constructor()', () => {
  test('Returns instance of BypassCache', async () => {
    cache = new BypassCache();
    expect(cache).toBeInstanceOf(BypassCache);
  });
});

describe('get()', () => {
  test('Returns null', async () => {
    expect(await cache.get()).toBeNull();
  });
});

describe('set()', () => {
  test('Returns void', async () => {
    expect(await cache.set()).toBe(undefined);
  });
});

describe('getSet()', () => {
  test('Returns input value', async () => {
    expect(await cache.getSet('', {}, 0, async () => 'someValue')).toBe('someValue');
  });
});

describe('getObject()', () => {
  test('Returns null', async () => {
    expect(await cache.getObject()).toBeNull();
  });
});

describe('setObject()', () => {
  test('Returns void', async () => {
    expect(await cache.setObject()).toBe(undefined);
  });
});

describe('getSetObject()', () => {
  test('Returns input value', async () => {
    expect(await cache.getSetObject('', {}, 0, async () => ({ value: 'someValue' }))).toMatchObject({ value: 'someValue' });
  });
});

describe('delete()', () => {
  test('Returns 0', async () => {
    expect(await cache.delete()).toBe(0);
  });
});

describe('disconnect()', () => {
  test('Returns void', async () => {
    expect(await cache.disconnect()).toBe(undefined);
  });
});
