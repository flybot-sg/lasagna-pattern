# Pull Pattern Playground

An interactive browser-based playground for experimenting with the pull pattern DSL.

## Quick Start

### Frontend Only (Local Mode)

```bash
cd examples/pull-playground

# Install dependencies
npm install

# Start development server
npm run dev

# Open http://localhost:3000
```

### With Demo Server (Remote Mode)

```bash
# Terminal 1: Start demo server
cd examples/pull-playground
clj -M:run              # Server on :8081

# Terminal 2: Start frontend
npm run dev             # Frontend on :3000
```

## Usage

### Local Mode

1. Enter a pull pattern in the Pattern editor
2. Enter EDN data in the Data editor
3. Click Execute to see matched data and variable bindings

### Remote Mode

1. Switch to Remote mode
2. Enter the server URL (default: http://localhost:8081/api)
3. Enter a pull pattern
4. Click Execute to query the demo server

## Example Patterns

| Pattern | Description |
|---------|-------------|
| `{:name ?n}` | Bind `:name` value to `n` |
| `{:name ?n :age ?a}` | Extract multiple values |
| `{:user {:name ?n}}` | Match nested maps |
| `[?first ?rest*]` | Sequence destructuring |
| `(?x :when pos?)` | Predicate constraint |
| `(?x :default 0)` | Default value |

## Demo Server Data

The demo server provides sample data:

```clojure
{:users [{:id 1 :name "Alice" :email "alice@example.com" :role :admin}
         {:id 2 :name "Bob" :email "bob@example.com" :role :user}
         {:id 3 :name "Carol" :email "carol@example.com" :role :user}]
 :posts [{:id 1 :title "Hello World" :author "Alice" :tags ["intro"]}
         {:id 2 :title "Pattern Matching" :author "Bob" :tags ["tutorial"]}]
 :config {:version "1.0.0" :features {:dark-mode true}}}
```

Example queries:
- `{:users ?all}` - Get all users
- `{:posts {{:id 1} ?post}}` - Get post by ID (indexed lookup)
- `{:config {:version ?v}}` - Extract config version
