var _marked = /*#__PURE__*/regeneratorRuntime.mark(capitalizeTwoWords),
  _marked2 = /*#__PURE__*/regeneratorRuntime.mark(capitalizeWord);
function capitalizeTwoWords(string) {
  var words, cw1, cw2;
  return regeneratorRuntime.wrap(function capitalizeTwoWords$(_context) {
    while (1) switch (_context.prev = _context.next) {
      case 0:
        words = string.split(' ');
        _context.next = 3;
        return capitalizeWord(words[0]);
      case 3:
        cw1 = _context.sent;
        _context.next = 6;
        return capitalizeWord(words[1]);
      case 6:
        cw2 = _context.sent;
        return _context.abrupt("return", cw1 + ' ' + cw2);
      case 8:
      case "end":
        return _context.stop();
    }
  }, _marked);
}
function capitalizeWord(word) {
  return regeneratorRuntime.wrap(function capitalizeWord$(_context2) {
    while (1) switch (_context2.prev = _context2.next) {
      case 0:
        return _context2.abrupt("return", word[0].toUpperCase() + word.slice(1));
      case 1:
      case "end":
        return _context2.stop();
    }
  }, _marked2);
}
