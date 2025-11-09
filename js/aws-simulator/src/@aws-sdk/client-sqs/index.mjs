export class SQSClient {
    send(command) {
        console.log("SQSClient send called with command:", command);
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

}

export class DeleteMessageCommand extends SQSCommand {
}

export class ReceiveMessageCommand extends SQSCommand {

}

