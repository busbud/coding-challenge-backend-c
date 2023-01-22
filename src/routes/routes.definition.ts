export enum MethodType {
  GET = "get",
  PUT = "put",
  POST = "post",
  DELETE = "delete",
}

export interface RouteType {
  method: MethodType;
  path: string;
  handler: any;
  public?: boolean;
}
