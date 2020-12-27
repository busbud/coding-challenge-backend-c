import { SuggestionsServiceConfiguration } from './suggestions.service';
import { ConfigService } from '@nestjs/config';

const DEFAULT_WEIGHTS = {
  population: 0.3,
  criteria: 0.6,
  nearBy: 0.1,
};

type Weights = {
  population: number;
  criteria: number;
  nearBy: number;
};

export default function (c: ConfigService): SuggestionsServiceConfiguration {
  function getWeights(): Weights {
    const env = c.get<string>('SUGGESTION_SCORING_WEIGHTS');
    if (!env) {
      return DEFAULT_WEIGHTS;
    }
    if (
      !/^population:(\d+\.?\d*),criteria:(\d+\.?\d*),nearBy:(\d+\.?\d*)$/.test(
        env,
      )
    ) {
      throw new Error(`Invalid SUGGESTION_SCORING_WEIGHTS syntax: ${env}`);
    }
    const weights: Record<string, number> = env
      .split(',')
      .reduce((acc, expression) => {
        const [name, weight] = expression.split(':');
        return { ...acc, [name]: parseFloat(weight) };
      }, {});
    const sum = Object.values(weights)
      .reduce((sum, it) => sum + it, 0)
      .toPrecision(10);
    if (sum != '1.000000000') {
      throw new Error(
        `Invalid SUGGESTION_SCORING_WEIGHTS values should sum to 1 but it sums to ${sum}`,
      );
    }
    return weights as Weights;
  }

  return {
    weights: getWeights(),
    reportReturned: c.get<boolean>('SUGGESTIONS_REPORT_RETURNED', true),
  };
}
