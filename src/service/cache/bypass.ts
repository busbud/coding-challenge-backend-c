import { CacheInterface } from 'service/cache';

export default class BypassCache implements CacheInterface {
  private object = null;

  async getObject() {
    return this.object;
  }

  async setObject() {
    this.object = null;
  }

  async getSetObject(_: string, __: number, freshObject: () => Promise<any>) {
    this.object = null;
    return freshObject();
  }

  async delete() {
    this.object = null;
    return 0;
  }

  async disconnect() {
    this.object = null;
  }
}
