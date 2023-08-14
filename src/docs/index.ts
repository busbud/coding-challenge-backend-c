import paths from './paths';
import components from './components';
import schemas from './schemas';

export default {
  openapi: '3.0.0',
  info: {
    title: 'Busbud Coding Challenge',
    description: '',
    version: '1.0.0',
  },
  externalDocs: {
    description: 'Link to the repository',
    url: 'https://github.com/rodrigovk/coding-challenge-backend-c',
  },
  // servers: [
  //   {
  //     url: '/',
  //     description: '',
  //   },
  // ],
  tags: [
    {
      name: 'Cities',
      description: '',
    },
  ],
  paths,
  schemas,
  components,
};
