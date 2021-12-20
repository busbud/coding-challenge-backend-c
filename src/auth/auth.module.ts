import {Module} from '@nestjs/common';
import {PassportModule} from '@nestjs/passport';
import {HeaderApiKeyStrategy} from './auth-header-api-key.strategy';
import {ConfigModule} from '@nestjs/config';

@Module({
    imports: [PassportModule, ConfigModule],
    providers: [HeaderApiKeyStrategy],
})
export class AuthModule {
}
