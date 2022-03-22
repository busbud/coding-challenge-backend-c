import { PrismaService } from "../prisma.service";
export declare class SuggestionsController {
    private prismaService;
    constructor(prismaService: PrismaService);
    getByName(q: any): Promise<{
        suggestions: any;
    }>;
}
