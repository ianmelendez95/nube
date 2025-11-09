import { v5 } from '@std/uuid';

Deno.test("capitalizeWord invoke directly", async () => {
    Deno.env.set("SQS_BASE_URL", "sqs:");

    const {handler} = await import('./capitalizeWord/index.mjs')
    handler({
        Records: [
            {
                body: JSON.stringify(["hello"]),
                messageAttributes: {
                    OriginRequestId: {

                    }
                }
            },
            {
                body: JSON.stringify(["world"])
            },
        ]
    });
});