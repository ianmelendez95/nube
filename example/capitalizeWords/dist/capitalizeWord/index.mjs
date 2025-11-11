import {
  dynamoClient,
  capitalizeWords
} from 'proxies'
import {PutItemCommand} from '@aws-sdk/client-dynamodb';

const responseTableName = 'response-table';

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

        const result = await capitalizeWord.apply(null, args);
        const requestId = message.messageAttributes?.OriginRequestId?.stringValue;

        await dynamoClient.send(new PutItemCommand({
          TableName: responseTableName,
          Item: {
            'request-id': { S: requestId || `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}` },
            result: { S: JSON.stringify(result) },
            timestamp: { N: Date.now().toString() },
            functionName: { S: 'capitalizeWord' }
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