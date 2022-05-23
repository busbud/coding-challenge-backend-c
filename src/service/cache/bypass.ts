import { CacheAdapter } from 'service/cache';

export default class BypassCache implements CacheAdapter {
  private NULL = null;

  async get() {
    return this.NULL;
  }

  async set() {
    this.NULL = null;
  }

  async getSet(_: string, __: any, ___: number, value: () => Promise<string>) {
    this.NULL = null;
    return value();
  }

  async getObject() {
    return this.NULL;
  }

  async setObject() {
    this.NULL = null;
  }

  async getSetObject(_: string, __: any, ___: number, value: () => Promise<any>) {
    this.NULL = null;
    return value();
  }

  async delete() {
    this.NULL = null;
    return 0;
  }

  async disconnect() {
    this.NULL = null;
  }
}
