# Structural Address Format for ScriptContext Test Cases

- Status: accepted
- Date: 2025-08-26
- Tags: testing, scriptcontext, address-format, schema, json

Technical Story: Introduce structural JSON address format for comprehensive ScriptContext testing in cape-tests.json files.

## Context and Problem Statement

The UPLC-CAPE test framework needed a robust way to specify addresses in ScriptContext test cases for comprehensive smart contract validation. Test cases require the ability to specify both script addresses (for contract-to-contract interactions) and public key addresses (for user interactions) with proper type discrimination and schema validation.

How should we specify addresses in cape-tests.json files to provide clear type safety, proper validation, and explicit address information for comprehensive ScriptContext testing?

## Decision Drivers

- **Type Safety**: Clear discrimination between script and pubkey addresses
- **Schema Validation**: Proper JSON schema support with structural validation
- **Explicit Specification**: All address information explicitly specified
- **Maintainability**: Easy to read, write, and validate test specifications
- **Cardano Compatibility**: Align with Cardano address model (script vs pubkey)
- **Reference Support**: Support for reusable address definitions

## Considered Options

- **Simple string format**: Basic string representation with minimal structure
- **Structural JSON format**: Use discriminated union with explicit type and hash fields
- **Nested object approach**: Complex nested structure with multiple address components

## Decision Outcome

Chosen option: **Structural JSON format**, because it provides clear type discrimination, enables proper schema validation, supports reference resolution, and makes test specifications explicit and maintainable.

### Address Format Specification

**Script Address:**

```json
{
  "type": "script",
  "script_hash": "1111111111111111111111111111111111111111111111111111111111"
}
```

**Public Key Address:**

```json
{
  "type": "pubkey",
  "pubkey_hash": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
}
```

**Reference Support:**

```json
{
  "type": "pubkey",
  "pubkey_hash": "@buyer_pubkey"
}
```

### JSON Schema Definition

```json
"address_spec": {
  "type": "object",
  "discriminator": {
    "propertyName": "type"
  },
  "oneOf": [
    {
      "type": "object",
      "properties": {
        "type": { "const": "script" },
        "script_hash": {
          "type": "string",
          "pattern": "^[0-9a-fA-F]{56}$"
        }
      },
      "required": ["type", "script_hash"]
    },
    {
      "type": "object",
      "properties": {
        "type": { "const": "pubkey" },
        "pubkey_hash": {
          "type": "string",
          "description": "64-character hex string or @reference"
        }
      },
      "required": ["type", "pubkey_hash"]
    }
  ]
}
```

### Haskell Implementation

```haskell
data AddressSpec
  = ScriptAddressSpec Text
  | PubkeyAddressSpec Text
  deriving (Eq, Show)

instance FromJSON AddressSpec where
  parseJSON = withObject "AddressSpec" \o -> do
    addrType :: Text <- o .: "type"
    case addrType of
      "script" -> ScriptAddressSpec <$> o .: "script_hash"
      "pubkey" -> PubkeyAddressSpec <$> o .: "pubkey_hash"
      _ -> fail $ "Unknown address type: " <> toString addrType

parseAddressSpec :: Map Text DataStructureEntry -> AddressSpec -> IO V3.Address
parseAddressSpec dataStructures addressSpec = case addressSpec of
  ScriptAddressSpec scriptHashText ->
    pure $ V3.Address
      (V3.ScriptCredential (V3.ScriptHash (fromString (toString scriptHashText))))
      Nothing
  PubkeyAddressSpec pubkeyHashText -> do
    resolvedHash <- resolveReference dataStructures pubkeyHashText
    pure $ V3.Address
      (V3.PubKeyCredential (V3.PubKeyHash (fromString (toString resolvedHash))))
      Nothing
```

### Positive Consequences

- **Clear Type Safety**: Explicit discrimination between script and pubkey addresses
- **Schema Validation**: Comprehensive JSON schema validation with proper error messages
- **Self-Documenting**: Test specifications are clear about address types
- **Reference Support**: Reusable address definitions via @reference resolution
- **Runtime Safety**: Proper hex string conversion using OverloadedStrings
- **Extensibility**: Easy to add new address types or fields in the future

### Negative Consequences

- **Verbosity**: More verbose than simple string representations
- **Learning Curve**: Test authors need to understand the structural format

## Implementation Details

### Files Modified

1. **lib/Cape/Tests.hs**: Core parsing logic with new AddressSpec data type
2. **schemas/cape-tests.schema.json**: JSON schema with structural address format
3. **scenarios/two-party-escrow/cape-tests.json**: Example usage in test specifications

### Runtime Conversion

Uses OverloadedStrings extension for runtime hex string conversion:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- Converts hex string to BuiltinByteString at runtime
scriptHash = fromString (toString scriptHashText) :: BuiltinByteString
```

## Examples

### Script Address Usage

```json
{
  "op": "add_output_utxo",
  "address": {
    "type": "script",
    "script_hash": "1111111111111111111111111111111111111111111111111111111111"
  },
  "lovelace": 75000000
}
```

### Public Key Address with Reference

```json
{
  "op": "add_output_utxo",
  "address": {
    "type": "pubkey",
    "pubkey_hash": "@impostor_pubkey"
  },
  "lovelace": 75000000
}
```

### Data Structure Definitions

```json
{
  "data_structures": {
    "buyer_pubkey": {
      "type": "builtin_data",
      "value": "#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    }
  }
}
```

## Links

- Related to comprehensive ScriptContext testing capabilities
- Builds on custom BuiltinData text encoding (ADR-0005)
- Enables proper schema validation for complex test specifications
