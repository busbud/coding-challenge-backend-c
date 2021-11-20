import { Controller} from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';

@Controller(`/`)
@ApiTags(`root`)
export class AppController {

}
