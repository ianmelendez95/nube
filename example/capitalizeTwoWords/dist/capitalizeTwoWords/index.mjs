import {
  sqsClient,
  dynamoClient,
  capitalizeTwoWords_1,
  capitalizeTwoWords_2,
  capitalizeWord
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
        const result = JSON.stringify(await capitalizeTwoWords.apply(null, args), null, 2)

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
        body: JSON.stringify(await capitalizeTwoWords.apply(null, args), null, 2)
      }
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function capitalizeTwoWords(_ctx)  {
    const string = _ctx.args[0];
    _ctx.frame.words = string.split(' ');
    _ctx.frame.word1 = _ctx.frame.words[0];
    _ctx.frame.word2 = _ctx.frame.words[1];
    _ctx.call('capitalizeWord', [_ctx.frame.word1],'capitalizeTwoWords_1');
}