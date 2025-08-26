# Custom BuiltinData Text Encoding for Test Cases

- Status: accepted
- Date: 2025-08-20
- Tags: testing, builtin-data, parsing, unified-test-system

Technical Story: Support custom BuiltinData text encoding in cape-tests.json files for comprehensive test case specifications.

## Context and Problem Statement

The UPLC-CAPE framework requires a human-readable text format for specifying BuiltinData values in test cases. Plutus Core uses complex BuiltinData structures (constructors, lists, maps, bytestrings, integers) that need to be easily authored and maintained in JSON test specifications.

How should we encode BuiltinData values in test case specifications to balance readability, expressiveness, and parsing simplicity?

## Decision Drivers

- **Readability**: Test authors should be able to easily read and write complex data structures
- **Completeness**: Support all BuiltinData types (integers, bytestrings, constructors, lists, maps)
- **Parsing reliability**: Unambiguous grammar that can be parsed deterministically
- **JSON compatibility**: Values must fit within JSON string format
- **Maintenance**: Simple enough to implement and debug

## Considered Options

- **Custom compact syntax**: Develop domain-specific text encoding
- **CBOR hex encoding**: Use hex-encoded CBOR representation
- **Haskell Show format**: Use Plutus Core's Show instance format
- **JSON nested objects**: Represent structure as nested JSON

## Decision Outcome

Chosen option: **Custom compact syntax**, because it provides optimal balance of readability and expressiveness while remaining simple to parse.

### Custom BuiltinData Text Format Specification

**Grammar (BNF):**

```bnf
data         ::= integer | bytestring | constructor | list | map
integer      ::= ('-')? digit+
bytestring   ::= '#' hexdigit*
constructor  ::= integer '(' data* ')'
list         ::= '[' data* ']'
map          ::= '{' pair* '}'
pair         ::= data ':' data
digit        ::= '0'..'9'
hexdigit     ::= '0'..'9' | 'a'..'f' | 'A'..'F'
whitespace   ::= (' ' | '\t' | '\n' | '\r')*
```

**Examples:**

- Integers: `42`, `-123`
- ByteStrings: `#`, `#deadbeef`, `#A1B2C3`
- Constructors: `0()`, `1(42 #cafe)`, `0(1(2()))`
- Lists: `[]`, `[1 2 3]`, `[42 #beef 0()]`
- Maps: `{}`, `{1:42}`, `{#key:0() 42:#value}`

### Positive Consequences

- **High readability**: Test authors can easily understand complex data structures
- **Complete expressiveness**: All BuiltinData types are supported
- **Deterministic parsing**: Unambiguous grammar enables reliable parsing
- **Compact representation**: Efficient use of space in JSON files
- **Custom parser control**: Full control over error messages and parsing behavior

### Negative Consequences

- **Custom implementation required**: Need to maintain domain-specific parser
- **Learning curve**: Test authors must learn custom syntax
- **Additional dependency**: Parser adds to codebase complexity

## Implementation Details

The parser is implemented in `measure/app/App/BuiltinDataParser.hs` using Megaparsec:

- **Type**: `parseBuiltinDataText :: Text -> Either ParseError Data`
- **Whitespace handling**: Flexible whitespace between tokens
- **Error reporting**: Detailed parse error messages with position information
- **Hex validation**: Proper validation of hex strings with even length requirement

## Examples in Practice

**Two-party escrow redeemer:**

```json
{
  "type": "builtin_data",
  "value": "0(#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 1000 0())"
}
```

**Complex nested structure:**

```json
{
  "type": "builtin_data",
  "value": "0([1 2] {42:#74657374} 1(#cafe))"
}
```

## Links

- Related to unified test system implementation in issue #36
- Uses Megaparsec for robust parsing with good error reporting
