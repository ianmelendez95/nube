import { SQSClient } from '@aws-sdk/client-sqs';
import { SendMessageCommand } from '@aws-sdk/client-sqs';
import { DynamoDBClient, GetItemCommand, UpdateItemCommand, PutItemCommand } from '@aws-sdk/client-dynamodb';
import { DeleteItemCommand } from '@aws-sdk/client-dynamodb';

export const sqsClient = new SQSClient();
export const dynamoClient = new DynamoDBClient();

export class Context {
  state;
  args;

  frameId;
  frame;

  contFrameId;
  contFnName;
  contState;

  constructor(state, args, frameId, frame, contFrameId, contFnName, contState) {
    this.state = state;
    this.args = args;
    this.frameId = frameId;
    this.frame = frame;
    this.contFrameId = contFrameId;
    this.contFnName = contFnName;
    this.contState = contState;
  }

  static async fromHttpReq(body) {
    console.trace('Context.fromHttpReq', body);
    const args = parseHttpArgs(body);
    const frameItem = await Context.makeNewFrameItem(undefined, undefined);

    return Context.fromArgsAndFrameItem(0, args, frameItem);
  }

  static async fromMessage(message) {
    console.trace('Context.fromMessage', message);
    const frameId = message.messageAttributes.frameId.stringValue;
    const frameItem = await Context.getFrameItem(frameId);

    const args = JSON.parse(message.body);

    const state = parseInt(message.messageAttributes.state.stringValue);

    return Context.fromArgsAndFrameItem(state, args, frameItem);
  }

  static fromArgsAndFrameItem(state, args, frameItem) {
    console.trace('Context.fromArgsAndFrameItem', state, args, frameItem);
    return new Context(
      state,
      args,
      frameItem.frameId.S,
      JSON.parse(frameItem.frame.S),
      frameItem.contFrameId?.S,
      frameItem.contFnName?.S,
      frameItem.contState?.N
    )
  }

  /**
   * Call with current continuation.
   * Essentially, call the function and when it 'returns', call with the current frame.
   */
  async callCC(fnName, args, contFnName, contState) {
    console.trace('Context.callCC', fnName, args, contFnName, contState);
    await this.saveFrame(); // commit current frame updates

    // 'push' the current frame onto the 'stack' (even in this stretched analogy it's more of a 'heap' but whatever)
    const nextFrameItem = await Context.makeNewFrameItem(
      this.frameId,
      contFnName,
      contState
    );

    // invoke the function by sending a message to its SQS queue
    return Context.invoke(fnName, args, nextFrameItem.frameId.S, 0);
  }

  /**
   * 'Return', which is really just calling the current continuation with the result.
   */
  async return(result) {
    console.trace('Context.return', result);
    if (this.contFnName) {
      // we have a continuation, so continue on that continuation
      return Context.invoke(this.contFnName, [result], this.contFrameId, this.contState);
    } else {
      // no continuation, meaning this is the root, so we are done.
      // save the result
      return Context.saveResult(this.frameId, result);
    }
  }

  async saveFrame() {
    return Context.updateFrame(this.frameId, this.frame);
  }

  static async invoke(fnName, args, frameId, state) {
    console.trace('Context.invoke', fnName, args, frameId, state);
    const queueUrl = `${process.env.SQS_BASE_URL}/${fnName}-request-queue`;
    console.trace('Context.invoke queueUrl', queueUrl);
    const body = JSON.stringify(args);
    console.trace('Context.invoke serialized args', body)
    return sqsClient.send(new SendMessageCommand({
      QueueUrl: queueUrl,
      MessageAttributes: {
        frameId: {
          DataType: 'String',
          StringValue: frameId,
        },
        state: {
          DataType: 'Number',
          StringValue: state.toString()
        }
      },
      MessageBody: body
    }));
  }

  static async deleteFrame(frameId) {
    console.trace('Context.deleteFrame', frameId);
    return dynamoClient.send(new DeleteItemCommand({
      TableName: 'frame-table',
      Key: {
        frameId: { S: frameId }
      }
    }));
  }

  static async updateFrame(frameId, frame) {
    console.trace('Context.updateFrame', frameId, frame);
    return dynamoClient.send(new UpdateItemCommand({
      TableName: 'frame-table',
      Key: {
        frameId: { S: frameId }
      },
      UpdateExpression: 'SET frame = :frame',
      ExpressionAttributeValues: {
        ':frame': { S: JSON.stringify(frame) }
      }
    }));
  }

  static async makeNewFrameItem(contFrameId, contFnName, contState) {
    console.trace('Context.makeNewFrameItem', contFrameId, contFnName, contState);
    const newFrame = {
      frameId: { S: crypto.randomUUID() },
      frame: { S: "{}" }
    }

    if (contFrameId) {
      newFrame.contFrameId = { S: contFrameId };
    }

    if (contFnName) {
      newFrame.contFnName = { S: contFnName };
    }

    if (contState) {
      newFrame.contState = { N: contState.toString() };
    }

    await dynamoClient.send(new PutItemCommand({
      TableName: 'frame-table',
      Item: newFrame
    }));

    return newFrame;
  }

  static async getFrameItem(frameId) {
    console.trace('Context.getFrameItem', frameId);
    const getItemResponse = await dynamoClient.send(new GetItemCommand({
      TableName: 'frame-table',
      Key: {
        frameId: { S: frameId }
      }
    }));

    if (!getItemResponse.Item) {
      throw new Error("Frame not found: " + frameId);
    }

    return getItemResponse.Item;
  }

  static async saveResult(frameId, result) {
    console.trace('Context.saveResult', frameId, result);
    return await dynamoClient.send(new PutItemCommand({
      TableName: 'result-table',
      Item: {
        frameId: { S: frameId },
        result: { S: JSON.stringify(result) }
      }
    }));
  }
}

export const eventHandler = (fn) => async (event) => {
  try {
    if (event.Records) {
      console.info(`Processing ${event.Records.length} messages.`);

      await Promise.all(event.Records.map(message =>
        Context.fromMessage(message).then(fn)
      ));
    } else {
      console.info('Processing request event');

      if (event.queryStringParameters?.check) {
        const result = await checkResult(event.queryStringParameters.check);
        return result
          ? { statusCode: 200, body: result }
          : { statusCode: 202 }
      }

      const ctx = await Context.fromHttpReq(event.body);
      await fn(ctx);

      return {
        statusCode: 200,
        body: `Result frameId: ${ctx.frameId}`
      }
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

export function parseHttpArgs(argsString) {
  console.trace('parseHttpArgs', argsString);
  let args = (typeof argsString === 'undefined' || argsString.trim().length === 0)
    ? []
    : JSON.parse(argsString)

  if (!Array.isArray(args)) {
    args = [args]
  }

  return args;
}

async function checkResult(frameId) {
  const getItemResponse = await dynamoClient.send(new GetItemCommand({
    TableName: 'result-table',
    Key: {
      frameId: { S: frameId }
    }
  }));

  return getItemResponse?.Item?.result?.S;
}
