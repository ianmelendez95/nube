
export class SQSClient {

}

export class DeleteMessageCommand {
}

export type SQSCreateQueueResponse = {
    url: string;
}

type SQSQueue = {
    messages: SQSMessage[]
}

type SQSMessage = {
    body: string;
}

export class SQS {
    _interval: number = 0;

    _queues: Map<string, SQSQueue> = new Map();

    createQueue(name: string) {
        if (this._queues.has(name)) {
            throw new Error(`Queue ${name} already exists`);
        }

        this._queues.set(name, { messages: [] });
    }

    sendMessage(queueName: string, messageBody: string) {
        const queue = this._queues.get(queueName);
        if (!queue) {
            throw new Error(`Queue ${queueName} does not exist`);
        }

        queue.messages.push({ body: messageBody });
    }

    receiveMessage(queueName: string): SQSMessage[] {
        const queue = this._queues.get(queueName);
        if (!queue) {
            throw new Error(`Queue ${queueName} does not exist`);
        }

        const messages = queue.messages;
        queue.messages = [];
        return messages;
    }
}


