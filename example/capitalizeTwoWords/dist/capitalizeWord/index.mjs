import {
  sqsClient,
  dynamoClient,
  capitalizeTwoWords_2,
  capitalizeTwoWords_1,
  capitalizeTwoWords
} from 'proxies'
import {PutItemCommand} from '@aws-sdk/client-dynamodb';

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

        const requestId = message.messageAttributes.OriginRequestId.stringValue;
        const result = JSON.stringify(await capitalizeWord.apply(null, args), null, 2)

        await dynamoClient.send(new PutItemCommand({
          TableName: 'response-table',
          Item: {
            'request-id': { S: requestId },
            result: { S: result },
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

async function capitalizeWord(_ctx)  {
    const word = _ctx.args[0];
    const result = word[0].toUpperCase() + word.slice(1)
    _ctx.return(result);
}