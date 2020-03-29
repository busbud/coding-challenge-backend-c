import { Client } from '@elastic/elasticsearch';

import config from './config';

const client = new Client(config.esClientConfig);

export default client;
