function capitalizeTwoWords(string) {
  switch (state) {
    case 0:
      const words = string.split(' ');
      const word1 = words[0];
      const word2 = words[1];
      return _ctx.call('capitalizeWord', [word1], 'capitalizeTwoWords', 1);
    case 1:
      const capitalizedWord1 = _ctx.args[0];
      return _ctx.call('capitalizeWord', [word2], 'capitalizeTwoWords', 2);
    case 2:
      const capitalizedWord2 = _ctx.args[0];
      return _ctx.return(capitalizedWord1 + ' ' + capitalizedWord2);
  }
}
