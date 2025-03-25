module.exports = grammar({
  name: 'parallax',

  // Handle ambiguity between expressions and struct expressions
  conflicts: $ => [
    [$.or_pattern, $.or_pattern],
    [$.function_type, $.kind_app],  // Add explicit conflict between function types and kind apps
    [$.expression, $.struct_expr],  // Add conflict between expressions and struct expressions
  ],

  // Corresponds to operator precedence in README.md EBNF:
  // BinaryOp ::= ArrowOp | LogicalOrOp | LogicalAndOp | ComparisonOp | AdditionOp | MultiplicationOp | CastOp
  precedences: $ => [
    [
      'parameter',      // For Parameter ::= "self" | "..."? Identifier ":" Type ("=" Expr)?
      'let',            // For LetExpr ::= "let" Pattern (":" Type)? "=" Expression
      'or_pattern',     // For OrPattern ::= Pattern ("|" Pattern)*
      'pattern',        // For Pattern ::= Identifier | Literal | Constructor | ...
      'match',          // For MatchExpr ::= "match" Expression "{" MatchArms? "}"
      'struct_literal', // For StructExpr ::= Path StructBody
      'path',          // For Path ::= "::"? PathSegment ("::" PathSegment)*
      'if',            // For IfExpr ::= "if" Expression "then" Expression ("else" Expression)?
      'lambda',        // For LambdaExpr ::= GenericParameters? "|" Parameters? "|" "=>" Expression
      'call',          // For CallExpr ::= Expression "(" CallArgs? ")"
      'field',         // For FieldAccess ::= Expression "." (Identifier | DecimalLiteral)
      'unary',         // For UnaryExpr ::= UnaryOp Expression
      'cast',          // For CastOp ::= "as"
      'mult',          // For MultiplicationOp ::= "*" | "/" | "%" | "&" | "<<" | ">>"
      'add',           // For AdditionOp ::= "+" | "-" | "|" | "^"
      'compare',       // For ComparisonOp ::= "==" | "!=" | "<" | "<=" | ">" | ">="
      'and',           // For LogicalAndOp ::= "&&"
      'or',            // For LogicalOrOp ::= "||"
      'arrow',         // For ArrowOp ::= "->"
      'function_type', // For FunctionType ::= Type "->" Type
      'function_kind', // For FunctionKind ::= Kind "->" Kind
      'where',         // For WhereClause ::= "where" WherePred ("," WherePred)* ","?
      'kind_app'       // For KindApp ::= Type "<" Type ("," Type)* ">"
    ]
  ],

  // Corresponds to Identifier ::= [a-zA-Z_][a-zA-Z0-9_]* in EBNF
  word: $ => $.identifier,

  // Corresponds to PathSegment in EBNF
  inline: $ => [
    $.path_segment
  ],

  // Corresponds to comment rules (not explicitly in EBNF but mentioned in examples)
  extras: $ => [
    /\s/,
    $.comment
  ],

  rules: {
    // Corresponds to SourceFile ::= Item* in EBNF
    source_file: $ => repeat($.item),

    // Comment syntax (both line and block comments)
    comment: $ => choice(
      seq('//', /.*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/'
      )
    ),

    // Corresponds to Item ::= Visibility? (Function | TypeDef | Enum | Struct | Trait | Impl | Module | Use)
    item: $ => seq(
      optional($.visibility),
      choice(
        $.function,
        $.type_def,
        $.enum,
        $.struct,
        $.trait,
        $.impl,
        $.module,
        $.use
      )
    ),

    // Corresponds to Visibility ::= "pub"? in EBNF
    visibility: $ => 'pub',

    // Corresponds to Function ::= FunctionSig "=" Expr ";" in EBNF
    function: $ => seq(
      field('sig', $.function_sig),
      '=',
      field('body', $.expression),
      ';'
    ),

    // Corresponds to FunctionSig in EBNF
    function_sig: $ => seq(
      'fn',
      field('name', $.identifier),
      optional(field('generics', $.generic_parameters)),
      '(',
      optional(field('params', $.parameters)),
      ')',
      optional(seq('->', field('return_type', $.type))),
      optional(field('where_clause', $.where_clause))
    ),

    // Parameters
    parameters: $ => seq(
      $.parameter,
      repeat(seq(',', $.parameter)),
      optional(',')
    ),

    parameter: $ => prec.right('parameter', choice(
      seq(
        '...',
        field('name', $.identifier),
        optional(seq(':', field('type', $.type))),
        optional(seq('=', field('value', $.expression)))
      ),
      'self',
      seq(
        field('pattern', $.pattern),
        optional(seq(':', field('type', $.type))),
        optional(seq('=', field('value', $.expression)))
      )
    )),

    // Types
    type: $ => choice(
      prec.left('path', $.path),
      prec.right('function_type', $.function_type),
      $.tuple_type,
      prec.left(2, $.kind_app),  // Higher precedence than function_type
      $.array_type,
    ),

    function_type: $ => prec.right('function_type', seq(
      'fn',
      '(',
      optional(field('params', seq(
        $.type,
        repeat(seq(',', $.type)),
        optional(',')
      ))),
      ')',
      optional(seq('->', field('return', $.type)))
    )),

    tuple_type: $ => seq(
      '(',
      optional(seq(
        field('elements', seq(
          $.type,
          repeat(seq(',', $.type)),
          optional(',')
        ))
      )),
      ')'
    ),

    kind_app: $ => prec.left(2, seq(  // Same precedence as in type rule
      field('base', $.type),  // Allow any type as base, not just paths
      '<',
      field('args', seq(
        $.type,
        repeat(seq(',', $.type)),
        optional(',')
      )),
      '>'
    )),

    array_type: $ => seq(
      '[',
      field('element', $.type),
      ';',
      field('size', $.expression),
      ']'
    ),

    // Expressions
    expression: $ => choice(
      $.block,
      prec.right('match', $.match_expr),  // Give match expressions higher precedence
      $.if_expr,
      $.binary_expr,
      $.unary_expr,
      $.call_expr,
      $.lambda_expr,
      $.literal,
      $.path,
      $.field_access,
      $.array_expr,
      $.tuple_expr,
      $.map_expr,
      $.hashset_expr,
      $.let_expr,
      prec('struct_literal', $.struct_expr),
      seq('(', $.expression, ')')
    ),

    block: $ => seq(
      '{',
      repeat($.block_item),
      optional($.expression),
      '}'
    ),

    block_item: $ => choice(
      $.function,
      $.type_def,
      $.enum,
      $.struct,
      $.use,
      seq($.expression, ';')
    ),

    // Basic identifiers and literals
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // Path rules
    path: $ => prec.right('path', seq(
      optional('::'),
      $.path_segment,
      repeat(seq('::', $.path_segment))
    )),

    path_segment: $ => choice(
      $.identifier,
      'self',
      'super',
      'crate'
    ),

    literal: $ => choice(
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.character_literal,
      $.byte_char_literal,
      $.boolean_literal
    ),

    integer_literal: $ => choice(
      seq($.decimal_literal, optional($.integer_suffix)),
      seq($.hex_literal, optional($.integer_suffix)),
      seq($.octal_literal, optional($.integer_suffix)),
      seq($.binary_literal, optional($.integer_suffix))
    ),

    decimal_literal: $ => /[0-9][0-9_]*[0-9]?/,
    hex_literal: $ => /0x[0-9a-fA-F][0-9a-fA-F_]*[0-9a-fA-F]?/,
    octal_literal: $ => /0o[0-7][0-7_]*[0-7]?/,
    binary_literal: $ => /0b[0-1][0-1_]*[0-1]?/,

    integer_suffix: $ => choice(
      'i8', 'i16', 'i32', 'i64', 'i128', 'isize',
      'u8', 'u16', 'u32', 'u64', 'u128', 'usize'
    ),

    float_literal: $ => seq(
      choice($.decimal_float, $.exponent_float),
      optional($.float_suffix)
    ),

    decimal_float: $ => /[0-9][0-9_]*[0-9]?\.[0-9][0-9_]*[0-9]?/,
    exponent_float: $ => seq(
      choice($.decimal_float, $.decimal_literal),
      /[eE][+-]?/,
      $.decimal_literal
    ),

    float_suffix: $ => choice('f32', 'f64'),

    // String literals
    string_literal: $ => choice(
      $.raw_string_literal,
      $.normal_string_literal,
      $.byte_string_literal
    ),

    raw_string_literal: $ => seq(
      'r',
      field('hashes', repeat('#')),
      '"',
      field('content', /([^"]|"[^#])*?/), // Any chars except quote, or quote not followed by enough #s
      '"',
      field('closing_hashes', repeat('#'))
    ),

    normal_string_literal: $ => seq(
      '"',
      repeat(choice(
        token.immediate(prec(1, /[^\\"\n]+/)),  // Regular string content
        $.string_escape
      )),
      '"'
    ),

    byte_string_literal: $ => seq(
      'b"',
      repeat(choice(
        token.immediate(prec(1, /[^\\"\n\x80-\xff]+/)),  // ASCII only
        $.byte_escape
      )),
      '"'
    ),

    string_escape: $ => token.immediate(choice(
      /\\x[0-9a-fA-F]{2}/,                // \xHH        2 hex digits
      /\\u\{[0-9a-fA-F]{1,6}\}/,          // \u{H...}    1-6 hex digits
      /\\n/,                               // \n          newline
      /\\r/,                               // \r          carriage return
      /\\t/,                               // \t          tab
      /\\0/,                               // \0          null
      /\\"/,                               // \"          quote
      /\\\\/,                              // \\          backslash
      /\\\n\s*/                            // line continuation
    )),

    byte_escape: $ => token.immediate(choice(
      /\\x[0-9a-fA-F]{2}/,                // \xHH        2 hex digits
      /\\n/,                               // \n          newline
      /\\r/,                               // \r          carriage return
      /\\t/,                               // \t          tab
      /\\0/,                               // \0          null
      /\\"/,                               // \"          quote
      /\\\\/,                              // \\          backslash
      /\\\n\s*/                            // line continuation
    )),

    // Corresponds to CharacterLiteral ::= "'" Character "'" in EBNF
    character_literal: $ => seq(
      "'",
      choice(
        token.immediate(prec(1, /[^'\\]/)),         // Any single non-quote, non-backslash char
        $.char_escape
      ),
      "'"
    ),

    // Corresponds to CharacterEscape ::= "\" (HexEscape | UnicodeEscape | OctalEscape | SpecialEscape) in EBNF
    char_escape: $ => token.immediate(choice(
      /\\x[0-9a-fA-F]{2}/,                // \xHH        2 hex digits
      /\\u\{[0-9a-fA-F]{1,6}\}/,          // \u{H...}    1-6 hex digits
      /\\n/,                               // \n          newline
      /\\r/,                               // \r          carriage return
      /\\t/,                               // \t          tab
      /\\0/,                               // \0          null
      /\\'/,                               // \'          single quote
      /\\\\/                               // \\          backslash
    )),

    // Corresponds to ByteLiteral ::= "b'" Character "'" in EBNF
    byte_char_literal: $ => seq(
      "b'",
      choice(
        token.immediate(prec(1, /[^'\\\x80-\xff]/)), // Any single ASCII char
        $.byte_char_escape
      ),
      "'"
    ),

    // Corresponds to ByteEscape ::= "\" (HexEscape | OctalEscape | SpecialEscape) in EBNF
    byte_char_escape: $ => token.immediate(choice(
      /\\x[0-9a-fA-F]{2}/,                // \xHH        2 hex digits
      /\\n/,                               // \n          newline
      /\\r/,                               // \r          carriage return
      /\\t/,                               // \t          tab
      /\\0/,                               // \0          null
      /\\'/,                               // \'          single quote
      /\\\\/                               // \\          backslash
    )),

    // Corresponds to BooleanLiteral ::= "true" | "false" in EBNF
    boolean_literal: $ => choice('true', 'false'),

    // Corresponds to BinaryOp and its sub-operators in EBNF
    binary_op: $ => choice(
      '->',  // ArrowOp ::= "->"
      '||',  // LogicalOrOp ::= "||"
      '&&',  // LogicalAndOp ::= "&&"
      choice('==', '!=', '<', '<=', '>', '>='),  // ComparisonOp ::= "==" | "!=" | "<" | "<=" | ">" | ">="
      choice('+', '-', '|', '^'),  // AdditionOp ::= "+" | "-" | "|" | "^"
      choice('*', '/', '%', '&', '<<', '>>'),  // MultiplicationOp ::= "*" | "/" | "%" | "&" | "<<" | ">>"
      'as'   // CastOp ::= "as"
    ),

    // Corresponds to UnaryOp ::= "-" | "!" | "&" | "*" in EBNF
    unary_op: $ => choice('-', '!', '&', '*'),

    // Corresponds to GenericArgs ::= "<" (Type ("," Type)* ","?)? ">" in EBNF
    generic_args: $ => prec.dynamic(1, seq(
      '<',
      optional(seq(
        $.type,
        repeat(seq(',', $.type)),
        optional(',')
      )),
      '>'
    )),

    // Type definitions
    type_def: $ => seq(
      'type',
      field('name', $.identifier),
      optional(field('generic_params', $.generic_parameters)),
      '=',
      field('value', $.type),
      optional(field('where_clause', $.where_clause)),
      ';'
    ),

    // Enums
    enum: $ => seq(
      'enum',
      field('name', $.identifier),
      optional(field('generic_params', $.generic_parameters)),
      optional(field('where_clause', $.where_clause)),
      '{',
      optional($.enum_variants),
      '}'
    ),

    enum_variants: $ => seq(
      $.enum_variant,
      repeat(seq(',', $.enum_variant)),
      optional(',')
    ),

    enum_variant: $ => seq(
      field('visibility', optional($.visibility)),
      field('name', $.identifier),
      optional(choice(
        field('struct_body', $.struct_body),
        field('tuple_body', $.tuple_body)
      ))
    ),

    // Structs
    struct: $ => seq(
      'struct',
      field('name', $.identifier),
      optional(field('generic_params', $.generic_parameters)),
      optional(field('where_clause', $.where_clause)),
      choice(
        field('body', $.struct_body),
        field('tuple_body', $.tuple_body),  // Add support for tuple structs
        ';'  // Support for unit structs
      )
    ),

    struct_body: $ => seq(
      '{',
      optional(seq(
        $.struct_field,
        repeat(seq(',', $.struct_field)),
        optional(',')
      )),
      '}'
    ),

    struct_field: $ => seq(
      field('visibility', optional($.visibility)),
      field('name', $.identifier),
      ':',
      field('type', $.type)
    ),

    tuple_body: $ => seq(
      '(',
      optional(seq(
        $.type,
        repeat(seq(',', $.type)),
        optional(',')
      )),
      ')'
    ),

    // Traits
    trait: $ => seq(
      'trait',
      field('name', $.identifier),
      optional(field('generic_params', $.generic_parameters)),
      optional(field('where_clause', $.where_clause)),
      optional(seq(':', field('bounds', $.trait_bounds))),
      '{',
      optional(field('items', $.trait_items)),
      '}'
    ),

    trait_bounds: $ => seq(
      $.path,
      repeat(seq('+', $.path))
    ),

    trait_items: $ => repeat1($.trait_item),

    trait_item: $ => choice(
      seq(
        $.function_sig,
        optional(seq('=', field('default_impl', $.expression))),
        ';'
      ),
      seq(
        'type',
        field('name', $.identifier),
        ';'
      )
    ),

    // Implementations
    impl: $ => seq(
      'impl',
      optional($.generic_parameters),
      $.type,
      optional(seq('for', $.type)),
      optional($.where_clause),
      '{',
      optional($.impl_items),
      '}'
    ),

    impl_items: $ => repeat1($.impl_item),

    impl_item: $ => choice(
      $.function,
      seq(
        field('sig', $.function_sig),
        field('body', $.block)
      ),
      seq(
        'type',
        field('name', $.identifier),
        '=',
        field('type', $.type),
        ';'
      )
    ),

    // Modules
    module: $ => seq(
      'mod',
      $.identifier,
      '{',
      optional(repeat($.item)),
      '}'
    ),

    // Use statements
    use: $ => seq(
      'use',
      field('tree', $.use_tree),
      ';'
    ),

    // Use tree structure for use declarations
    use_tree: $ => choice(
      seq(
        field('segment', $.path_segment),
        optional(seq('as', field('alias', $.identifier))),
        optional(field('sub_tree', seq('::', $.use_tree)))
      ),
      seq(
        '{',
        optional(seq(
          $.use_tree,
          repeat(seq(',', $.use_tree)),
          optional(',')
        )),
        '}'
      ),
      '*'
    ),

    // Generic parameters and where clauses
    generic_parameters: $ => seq(
      '<',
      optional(seq(
        $.generic_param,
        repeat(seq(',', $.generic_param)),
        optional(',')
      )),
      '>'
    ),

    generic_param: $ => seq(
      optional('phantom'),
      $.identifier,
      optional(seq(':', $.kind))
    ),

    kind: $ => choice(
      '*',
      prec.right('function_kind', $.function_kind),
      $.tuple_kind
    ),

    function_kind: $ => prec.right('function_kind', seq(
      field('param', $.kind),
      '->',
      field('return', $.kind)
    )),

    tuple_kind: $ => seq(
      '(',
      optional(seq(
        $.kind,
        repeat(seq(',', $.kind)),
        optional(',')
      )),
      ')'
    ),

    where_clause: $ => prec.right('where', seq(
      'where',
      field('predicates', seq(
        $.where_pred,
        repeat(seq(',', $.where_pred)),
        optional(',')
      ))
    )),

    where_pred: $ => seq(
      field('type', $.type),
      ':',
      field('bounds', seq(
        $.path,
        repeat(seq('+', $.path))
      ))
    ),

    // Expression rules that were referenced but not defined
    if_expr: $ => prec.right('if', seq(
      'if',
      field('condition', $.expression),
      'then',
      field('then_branch', $.expression),
      optional(seq('else', field('else_branch', $.expression)))
    )),

    // Corresponds to MatchExpr ::= "match" Expression "{" MatchArms? "}" in EBNF
    match_expr: $ => prec.right(10, seq(  // Give match_expr a much higher precedence
      'match',
      field('scrutinee', $.expression),
      '{',
      optional(field('arms', $.match_arms)),
      '}'
    )),

    match_arms: $ => seq(
      $.match_arm,
      repeat(seq(',', $.match_arm)),
      optional(',')
    ),

    match_arm: $ => seq(
      field('pattern', $.pattern),
      '=>',
      field('body', $.expression)
    ),

    unary_expr: $ => prec.left('unary', seq(
      field('op', $.unary_op),
      field('expr', $.expression)
    )),

    call_expr: $ => prec.right('call', seq(
      field('callee', $.expression),
      '(',
      optional(seq(
        field('args', $.argument),
        repeat(seq(
          ',',
          field('args', $.argument)
        )),
        optional(',')
      )),
      ')'
    )),

    argument: $ => choice(
      field('positional', $.expression),
      seq(
        field('name', $.identifier),
        ':',
        field('value', $.expression)
      ),
      seq('...', field('spread', $.expression))
    ),

    lambda_expr: $ => prec.right('lambda', seq(
      optional($.generic_parameters),
      '|',
      optional($.parameters),
      '|',
      '=>',
      $.expression
    )),

    // Corresponds to FieldAccess ::= Expression "." (Identifier | DecimalLiteral) in EBNF
    field_access: $ => prec.right('field', seq(
      field('object', $.expression),
      '.',
      field('field', choice($.identifier, $.decimal_literal))
    )),

    // Corresponds to ArrayExpr ::= "[" (Expression ("," Expression)* ","?)? "]" in EBNF
    array_expr: $ => seq(
      '[',
      optional(seq(
        $.expression,
        repeat(seq(',', $.expression)),
        optional(',')
      )),
      ']'
    ),

    // Corresponds to TupleExpr in EBNF
    tuple_expr: $ => seq(
      '(',
      optional(choice(
        seq($.expression, repeat1(seq(',', $.expression)), optional(',')),  // (a, b, c)
        seq($.expression, ',')  // (a,)
      )),
      ')'
    ),

    // Corresponds to MapExpr ::= "#" "{" (MapEntry ("," MapEntry)* ","?)? "}" in EBNF
    map_expr: $ => seq(
      '#',
      '{',
      optional(seq(
        $.map_entry,
        repeat(seq(',', $.map_entry)),
        optional(',')
      )),
      '}'
    ),

    // Corresponds to HashSetExpr ::= "#" "[" (Expression ("," Expression)* ","?)? "]" in EBNF
    hashset_expr: $ => seq(
      '#',
      '[',
      optional(seq(
        $.expression,
        repeat(seq(',', $.expression)),
        optional(',')
      )),
      ']'
    ),

    // Corresponds to MapEntry ::= Expression ":" Expression in EBNF
    map_entry: $ => seq($.expression, ':', $.expression),

    // Corresponds to LetExpr ::= "let" Pattern (":" Type)? "=" Expression in EBNF
    let_expr: $ => prec('let', seq(
      'let',
      field('pattern', $.pattern),
      optional(seq(':', field('type', $.type))),
      '=',
      field('value', $.expression)
    )),

    // Corresponds to StructExpr ::= Path StructBody in EBNF
    struct_expr: $ => prec.left('struct_literal', seq(
      field('path', $.path),
      field('body', $.expr_struct_body)
    )),

    // Corresponds to ExprStructBody in EBNF
    expr_struct_body: $ => seq(
      '{',
      optional(seq(
        field('fields', seq(
          $.field_init,
          repeat(seq(',', $.field_init)),
          optional(',')
        )),
        optional(field('base', $.base_struct))
      )),
      '}'
    ),

    // Corresponds to FieldInit ::= Identifier ":" Expression | Identifier in EBNF
    field_init: $ => choice(
      seq(field('name', $.identifier), ':', field('value', $.expression)),
      field('shorthand', $.identifier)
    ),

    // Corresponds to BaseStruct ::= ".." Expression in EBNF
    base_struct: $ => seq('..', field('expr', $.expression)),

    // Corresponds to Pattern grammar rules in EBNF
    pattern: $ => prec('pattern', choice(
      $.identifier,
      $.literal,
      $.constructor,
      $.array_pattern,
      $.tuple_pattern,
      $.rest_pattern,
      prec.left('or_pattern', $.or_pattern),
      $.wildcard_pattern,
      $.struct_pattern
    )),

    // Corresponds to Constructor ::= @Path (TuplePattern | StructPattern) in EBNF
    constructor: $ => seq(
      field('path', $.path),
      field('args', choice($.tuple_pattern, $.struct_pattern))
    ),

    // Corresponds to TuplePattern ::= "(" (Pattern ("," Pattern)* ","?)? ")" in EBNF
    tuple_pattern: $ => seq(
      '(',
      optional(field('elements', seq(
        $.pattern,
        repeat(seq(',', $.pattern)),
        optional(',')
      ))),
      ')'
    ),

    // Corresponds to ArrayPattern ::= "[" (Pattern ("," Pattern)* ","?)? "]" in EBNF
    array_pattern: $ => seq(
      '[',
      optional(field('elements', seq(
        $.pattern,
        repeat(seq(',', $.pattern)),
        optional(',')
      ))),
      ']'
    ),

    // Corresponds to StructPattern ::= "{" (FieldPattern ("," FieldPattern)* ","?)? "}" in EBNF
    struct_pattern: $ => seq(
      '{',
      optional(field('fields', seq(
        $.field_pattern,
        repeat(seq(',', $.field_pattern)),
        optional(',')
      ))),
      '}'
    ),

    // Corresponds to RestPattern ::= ".." in EBNF
    rest_pattern: $ => '..',

    // Corresponds to WildcardPattern ::= "_" in EBNF
    wildcard_pattern: $ => '_',

    // Corresponds to OrPattern ::= Pattern ("|" Pattern)* in EBNF
    or_pattern: $ => prec.left('or_pattern', seq(
      field('left', $.pattern),
      repeat1(seq(
        '|',
        field('right', $.pattern)
      ))
    )),

    // Corresponds to FieldPattern ::= Identifier (":" Pattern)? in EBNF
    field_pattern: $ => seq(
      field('name', $.identifier),
      optional(seq(':', field('pattern', $.pattern)))
    ),

    binary_expr: $ => {
      const table = [
        ['cast', ['as']],                                    // Highest precedence
        ['mult', ['*', '/', '%', '&', '<<', '>>']],
        ['add', ['+', '-', '|', '^']],
        ['compare', ['==', '!=', '<', '<=', '>', '>=']],
        ['and', ['&&']],
        ['or', ['||']],
        ['arrow', ['->']],                                   // Lowest precedence
      ];

      return choice(...table.map(([precedence, operators]) =>
        prec.left(precedence, seq(
          field('left', $.expression),
          field('op', choice(...operators)),
          field('right', $.expression)
        ))
      ));
    }
  }
}); 