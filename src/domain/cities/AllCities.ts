import Cities from "./Cities";

export default interface AllCities {
  inUSAAndCanadaWithMoreThan5000People(): Promise<Cities>;
}
