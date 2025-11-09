
async function main() {
    console.log("TRACE: ", process.env.NODE_PATH);
    process.env["SQS_BASE_URL"] = 'sqs:';

    const {handler} = await import('./src/capitalizeWords/capitalizeWord/index.mjs')
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
