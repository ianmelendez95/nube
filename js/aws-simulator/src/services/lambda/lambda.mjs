export default class Lambda {
    static _INSTANCE = new Lambda();

    _functions = new Map();

    static getInstance() {
        return Lambda._INSTANCE;
    }

    createFunction(name, handler) {
        if (this._functions.has(name)) {
            throw new Error(`Function ${name} already exists.`);
        }

        this._functions.set(name, handler);
    }

    getFunction(name) {
        if (!this._functions.has(name)) {
            throw new Error(`Function ${name} does not exist.`);
        }

        return this._functions.get(name);
    }
}