export class SQSClient {
    send(command: any) {
        console.log("SQSClient send called with command:", command);
    }
}

type SQSCommandInput = {[key: string]: any};

class SQSCommand {
    _command: SQSCommandInput;

    constructor(command: SQSCommandInput) {
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

