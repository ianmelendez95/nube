import {
  capitalizeWords
} from 'proxies'

const capitalizeWordResponseQueue = `${process.env.SQS_BASE_URL}/capitalizeWord-response-queue`;

export const handler = async (event) => {
  try {
    if (event.Records) {
      console.info(`Processing ${event.Records.length} messages.`);

      for (const message of event.Records) {
        let args = (typeof message.body === 'undefined' || message.body.trim().length === 0) 
          ? [] 
          : JSON.parse(message.body) 

        if (!Array.isArray(args)) {
          args = [args]
        }

        const result = JSON.stringify(await capitalizeWord.apply(null, args), null, 2)
        const requestId = message.messageAttributes?.OriginRequestId?.stringValue;

        await client.send(new SendMessageCommand({
          QueueUrl: capitalizeWordResponseQueue,
          MessageBody: result,
          MessageAttributes: {
            OriginRequestId: {
              DataType: 'String',
              StringValue: requestId
            }
          }
        }));
      }
    } else {
      console.info('Processing request event');

      let args = (typeof event.body === 'undefined' || event.body.trim().length === 0) 
        ? [] 
        : JSON.parse(event.body) 

      if (!Array.isArray(args)) {
        args = [args]
      }

      return {
        statusCode: 200,
        body: JSON.stringify(await capitalizeWord.apply(null, args), null, 2)
      }
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function capitalizeWord(word)  {
  return word[0].toUpperCase() + word.slice(1)
}