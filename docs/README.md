# Parallax Documentation

This directory contains the mdBook source for the Parallax Language Implementation guide.

## Building the Documentation

### Prerequisites

Install mdBook:
```bash
cargo install mdbook
```

### Building

To build the documentation:
```bash
mdbook build
```

The output will be in the `book/` directory.

### Development

To serve the documentation locally with auto-reload:
```bash
mdbook serve
```

Then open http://localhost:3000 in your browser.

### Structure

- `src/` - Markdown source files
  - `design/` - Language design documentation
  - `implementation/` - Implementation guide
  - `development/` - Development guide
  - `reference/` - Reference materials
  - `appendix/` - Additional resources
- `book/` - Generated HTML output (git-ignored)
- `book.toml` - mdBook configuration

## Contributing

When adding new chapters:
1. Create the markdown file in the appropriate directory
2. Add an entry to `src/SUMMARY.md`
3. Ensure the book builds without errors
4. Submit a pull request

## Style Guide

- Use ATX-style headers (`#` not underlines)
- Include code examples with proper syntax highlighting
- Keep lines under 100 characters when possible
- Use relative links between chapters
- Add language annotations to code blocks: ` ```parallax ` or ` ```rust `