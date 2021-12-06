import { Client } from '@elastic/elasticsearch';
import type { Client as NewTypes, ClientOptions } from '@elastic/elasticsearch/api/new';
import {
  IndicesCreateRequest,
  QueryDslQueryContainer,
  SearchHitsMetadata,
  SearchSortContainerKeys,
} from '@elastic/elasticsearch/api/types';

const { APP_ENV } = process.env;

export class ElasticSearchClient {
  private client: NewTypes;

  constructor(options?: ClientOptions) {
    // Default for localhost
    let esClientOptions: ClientOptions = { node: `http://localhost:${process.env.ES_PORT}`, ...options };

    // Quick and dirty for now - should probably find a common way to connect to local & cloud
    /* istanbul ignore if */
    if (APP_ENV === 'production') {
      esClientOptions = {
        cloud: {
          id: process.env.ES_CLOUD_ID!,
        },
        auth: {
          username: process.env.ES_CLOUD_USERNAME!,
          password: process.env.ES_CLOUD_PASSWORD!,
        },
      };
    }

    // @ts-expect-error @elastic/elasticsearch
    // https://www.elastic.co/guide/en/elasticsearch/client/javascript-api/current/typescript.html - not fully supported yet
    this.client = new Client(esClientOptions);
  }

  isExistentIndex = async (index: string): Promise<boolean> => {
    const { body: exists } = await this.client.indices.exists({ index });
    return exists;
  };

  createIndex = async (options: IndicesCreateRequest): Promise<number | null> => {
    const { statusCode } = await this.client.indices.create(options);
    return statusCode;
  };

  bulkInsert = async <T>(index: string, data: T[], docId?: keyof T) => {
    const body = data.flatMap((item: T) => [
      { index: { _index: index, _id: docId ? item[docId] : undefined } },
      item,
    ]);

    const { body: { errors }, statusCode } = await this.client.bulk({ body });
    return { errors, statusCode };
  };

  searchAsYouType = async <T>(
    index: string,
    field: string,
    term: string,
    options: { query?: QueryDslQueryContainer, sort?: SearchSortContainerKeys } = {},
  ): Promise<SearchHitsMetadata<T>> => {
    const { body: { hits } } = await this.client.search<T>({
      index,
      body: {
        query: {
          multi_match: {
            query: term,
            type: 'bool_prefix',
            fields: [field, `${field}.2gram`, `${field}.3gram`],
          },
          ...options.query,
        },
        sort: {
          ...options.sort,
        },
        size: 10,
      },
    });

    return hits;
  };
}
