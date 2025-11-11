import SQS from '../../sqs/sqs.mjs';

const sqs = SQS.getInstance();

export class SQSClient {
    send(command) {
        if (!command.QueueUrl) {
            throw new Error('QueueUrl is required.');
        }

        const queue = sqs.getQueue(command.QueueUrl);

        if (command instanceof SendMessageCommand) {
            queue.sendMessage(command._command)
        }
    }
}

class SQSCommand {
    _command;

    constructor(command) {
        this._command = command;
    }

    toString() {
        return JSON.stringify(this._command);
    }
}

export class SendMessageCommand extends SQSCommand {
    getMessage() {
        return this._command;
    }
}

export class DeleteMessageCommand extends SQSCommand {
}

export class ReceiveMessageCommand extends SQSCommand {

}

