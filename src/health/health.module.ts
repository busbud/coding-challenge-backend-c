import {Module} from '@nestjs/common';
import {TerminusModule} from '@nestjs/terminus';
import HealthController from './health.controller';

@Module({
    imports: [TerminusModule],
    controllers: [HealthController],
    providers: [],
})
export default class HealthModule {
}
