jest.mock('proxies', () => {
    return {
        sqsClient: {},
        capitalizeWords: function () {

        }
    }
}, {virtual: true});

jest.mock('@aws-sdk/client-sqs', () => {
    return {

    }
}, {virtual: true});

describe("capitalizeWord invoke directly", () => {
    test("capitalizeWord", async () => {
        console.log("TRACE: ", process.env.NODE_PATH);
        process.env["SQS_BASE_URL"] = 'sqs:';

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
});