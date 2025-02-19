# tree-sitter-parallax

This crate provides a [tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar for the [Parallax programming language](https://github.com/parallax-lang/parallax).

## Features

- Full syntax highlighting support
- Incremental parsing
- Error recovery
- Supports all Parallax language constructs:
  - Functions and lambdas
  - Structs and enums
  - Traits and implementations
  - Pattern matching
  - Generic types
  - Effect system annotations
  - Module system

## Installation

```bash
npm install tree-sitter-parallax
```

## Usage

### As a Development Dependency

```bash
npm install --save-dev tree-sitter-parallax
```

### In Your Code

```javascript
const Parser = require('tree-sitter');
const Parallax = require('tree-sitter-parallax');

const parser = new Parser();
parser.setLanguage(Parallax);

const sourceCode = `
fn main() -> Result<(), Error> = {
    println("Hello, parallel world!")?
};
`;

const tree = parser.parse(sourceCode);
console.log(tree.rootNode.toString());
```

### With Neovim

Add to your `init.lua`:

```lua
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.parallax = {
  install_info = {
    url = "https://github.com/parallax-lang/tree-sitter-parallax",
    files = {"src/parser.c"},
    branch = "main",
  },
  filetype = "plx",
}
```

## Development

### Prerequisites

- Node.js (>= 12.0.0)
- npm
- tree-sitter-cli (`npm install -g tree-sitter-cli`)

### Building

```bash
npm install
npm run build
```

### Testing

```bash
npm test
```

### Generating

```bash
tree-sitter generate
```

## License

MIT

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request 