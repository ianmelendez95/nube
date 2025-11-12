import _ from 'lodash';
import {
  eventHandler
} from 'proxies'

export const handler = eventHandler(capitalizeTwoWords_2);

async function capitalizeTwoWords_2(_ctx)  {
  _ctx.frame.capWord2 = _ctx.args[0];
  return _ctx.return(_ctx.frame.capWord1 + ' ' + _ctx.frame.capWord2);
}