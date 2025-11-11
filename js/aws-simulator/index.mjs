import SQS from './src/services/sqs/sqs.mjs';
import Lambda from './src/services/lambda/lambda.mjs';

const sqs = SQS.getInstance();
const lambda = Lambda.getInstance();

async function main() {
    process.env["SQS_BASE_URL"] = 'sqs:';

    const {handler: capitalizeWordHandler} = await import('./src/capitalizeWords/capitalizeWord/index.mjs')
    const {handler: capitalizeWordsHandler} = await import('./src/capitalizeWords/capitalizeWords/index.mjs')

    lambda.createFunction('capitalizeWord', capitalizeWordHandler);
    lambda.createFunction('capitalizeWords', capitalizeWordsHandler);

    const capitalizeWordRequestQueue = sqs.createQueue('capitalizeWord-request-queue');
    sqs.createQueue('capitalizeWord-response-queue');
    const capitalizeWordsRequestQueue = sqs.createQueue('capitalizeWords-request-queue');
    sqs.createQueue('capitalizeWords-response-queue');

    sqs.createSQSMapping(capitalizeWordRequestQueue, 'capitalizeWord');
    sqs.createSQSMapping(capitalizeWordsRequestQueue, 'capitalizeWords');

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

}

main()
