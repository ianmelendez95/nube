import {
  eventHandler
} from 'proxies'

export const handler = eventHandler(capitalizeTwoWords_2);

async function capitalizeTwoWords_2(_ctx)  {
    _ctx.return(_ctx.frame.word1 + ' ' + _ctx.frame.word2);
}