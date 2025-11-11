import Lambda from '../lambda/lambda.mjs';

const lambda = Lambda.getInstance();

export default class SQS {
    static _INSTANCE = new SQS();

    _queues = new Map();
    _sqsMappings = new Map();

    constructor() {
        this._queues = new Map();
    } 

    static getInstance() {
        return SQS._INSTANCE;
    }

    createQueue(name) {
        const url = `${process.env.SQS_BASE_URL}/${name}`;

        if (this._queues.has(url)) {
            throw new Error(`Queue ${url} already exists.`);
        }

        this._queues.set(url, new SQSQueue());

        return url;
    }

    getQueue(queueUrl) {
        if (!this._queues.has(queueUrl)) {
            throw new Error(`Queue ${queueUrl} does not exist.`);
        }

        return this._queues.get(queueUrl);
    }

    createSQSMapping(queueUrl, functionName) {
        this.getQueue(queueUrl).setFunctionName(functionName);
    }
}

export class SQSQueue {
    _messages = [];
    _functionName = null;

    setFunctionName(functionName) {
        if (this._functionName) {
            throw new Error(`Function ${this._functionName} already exists.`);
        }

        this._functionName = functionName;
    }

    getFunctionName() {
        return this._functionName;
    }

    sendMessage(message) {
        this._messages.push(message);
    }

    getMessages() {
        return this._messages;
    }
}