import { Request, Response, NextFunction } from 'express';
import { SearchResponse } from "elasticsearch";

export interface SuggestionsRequest {
  (
    request: Request & {
      query: {
        q: string,
        latitude: string,
        longitude: string,
        limit: string,
        offset: string,
      }
    },
    response: Response, 
    next: NextFunction
  ): void | Promise<void>
}

export type GeopointType = {
    lat: string | number,
    lon: string | number,
}

export type EsRecordType = {
  name: string,
  location: GeopointType
  score: number,
  [key: string]: any,
}

export interface EsSearchType {
  (
    filters: {
      term: string,
      location: {
        lat: string | number,
        lon: string | number,
      } | undefined,
      limit: string | number,
      offset: string | number,
    }
  ): Promise<SearchResponse<EsRecordType>>
}