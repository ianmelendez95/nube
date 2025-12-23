function capitalizeTwoWords(_ctx) {
  switch (_ctx.state) {
    case 0:
      _ctx.frame.string = _ctx.args[0];
      const words = string.split(' ');
      const word1 = words[0];
      const word2 = words[1];
      _ctx.callCC('capitalizeWord', [word1], 'capitalizeTwoWords', 1);
    case 1:
      _ctx.frame.capitalizedWord1 = _ctx.args[0];
      _ctx.callCC('capitalizeWord', [word2], 'capitalizeTwoWords', 2);
    case 2:
      _ctx.frame.capitalizedWord2 = _ctx.args[0];
      return capitalizedWord1 + ' ' + capitalizedWord2;
  }
}
