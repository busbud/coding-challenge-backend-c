import { TYPE } from 'inversify-restify-utils'

export const TYPES = {
    Logger: 'Logger',
    Controller: TYPE.Controller,
    SuggestionsController: 'SuggestionsController',
    Service: 'Service',
    SuggestionsService: 'SuggestionsService',
    IDatabaseRepository: 'IDatabaseRepository',
    FirestoreRepository: 'FirestoreRepository',
    Firestore: 'Firestore'
}
