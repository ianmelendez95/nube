import {
  eventHandler
} from 'proxies'

export const handler = eventHandler(capitalizeTwoWords_1);

async function capitalizeTwoWords_1(_ctx)  {
    _ctx.call('capitalizeWord', [_ctx.frame.word2], 'capitalizeTwoWords_2');
}