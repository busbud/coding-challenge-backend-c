import { RouteType, MethodType } from "./routes.definition";
import { CitiesController } from "../controllers";

export const routes: RouteType[] = [
  {
    path: "/suggestions",
    method: MethodType.GET,
    handler: CitiesController.citySuggestions,
    public: true,
  },
];
