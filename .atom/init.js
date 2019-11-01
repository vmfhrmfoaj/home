atom.keymaps.addKeystrokeResolver(({keystroke, event}) => {
  if (event.type !== 'keyup') {
    if (event.ctrlKey) {
      switch (event.code) {
        case 'BracketLeft':
          return 'escape';
        case 'KeyH':
          return 'backspace';
        case 'KeyM':
          return 'enter';
      }
    }
  }});
