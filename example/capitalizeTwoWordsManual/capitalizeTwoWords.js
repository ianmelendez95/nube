async function capitalizeTwoWords(_ctx) {
    const string = _ctx.args[0];
    _ctx.frame.words = string.split(' ');
    _ctx.frame.word1 = _ctx.frame.words[0];
    _ctx.frame.word2 = _ctx.frame.words[1];
    _ctx.call('capitalizeWord', [_ctx.frame.word1],'capitalizeTwoWords_1');
}

async function capitalizeTwoWords_1(_ctx) {
    _ctx.call('capitalizeWord', [_ctx.frame.word2], 'capitalizeTwoWords_2');
}

async function capitalizeTwoWords_2(_ctx) {
    _ctx.return(_ctx.frame.word1 + ' ' + _ctx.frame.word2);
}

async function capitalizeWord(_ctx) {
    const word = _ctx.args[0];
    const result = word[0].toUpperCase() + word.slice(1)
    _ctx.return(result);
}