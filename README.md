# JSON Query

A query library for JSON code, based on SRFI-180, implemented for Chicken Scheme.

See the [chez branch](https://github.com/JJK96/scheme-json-query/tree/chez) for a partial Chez port.

This library takes inspiration from [SXPath](https://metapaper.net/query/sxpath/) and [json-tools](https://github.com/ktakashi/json-tools/).

The repository also includes the script `jq`, which is (a very barebones) alternative to [jq](https://jqlang.org/).

## High-level overview

The library provides two main functions with the following definitions:

```
(json:query query)
(json:edit query)
```

Both use the same traverser logic. The traverser applies a rule to the input and then passes the result through to the next rule. There are two kinds of rules:

* Traversing rules: These rules return the result of the next rules in-place. For example, the `ref-rule` obtains the node or vector indicated by the given key and continues traversing the rest of the rules with this context. Once traversing is finished the result is returned in-place and the complete object is returned. This means that if the subsequent rules do not make any modifications, the complete invocation does not make any changes to the JSON. The traversing rules are: `ref-rule`, `(* ...)`
* Modifying rules: These rules modify the input and continue traversing the rest of the rules. The result of subsequent rules is returned as the complete object. The other rules are modifying.

The difference between `json:query` and `json:edit` is that `json:query` will return the result of the last rule, skipping the in-place modifications (by calling the continuation of the `json:query` invocation) while the `json:edit` function will return the results of each rule in-place. This allows replacing specific parts of the data with `json:edit` using a query that ends with a `replace` or other modifiying rule, while `json:query` allows retrieving a specific part of the data.

## Query language reference

The query argument to the above functions is a quoted S-expression with the following syntax:

```bnf
query ::= '(rule ...)

rule ::= procedure                        ; An unquoted procedure that takes as argument a node or vector of nodes, this can also be a recursive call to json:query. This procedure is applied to the node or nodes that result from the previously executed rule.
      | ref-rule                          ; Converted to json:ref
      | json-function-without-args        ; Reference to a json: procedure defined in this library. e.g. values -> json:values. These functions return a lambda that takes a node or vector of nodes as input.
      | function-rule                     ; Function rules are rules that take arguments.

ref-rule ::= string?    ; Return the value for the given key
          |  number?    ; Return the given index in the vector (JSON list)

json-function-without-args ::= ref      ; json:ref     (Get a value or vector-item from the given input)
                           |  keys      ; json:keys    (Get a vector of keys from the given input node)
                           |  values    ; json:values  (Get a vector of values from the given input node)
                           |  flatten   ; json:flatten (Flatten the given vector of vectors into a single vector)
                           |  unique    ; json:unique  (Remove duplicate objects from the given vector)
                           |  write     ; json:write   (Write the given S-expression back into JSON syntax)

function-rule ::= (* rule ...)                        ; For-each: Apply the given rules to each node in the vector of nodes that it receives.
               |  (*_ rule ...)                       ; For-each+flatten: Apply the given rules to each node in the vector of nodes that it receives, and afterwards flatten the result into a single vector.
               |  (filter procedure)                  ; Filter the nodes that it receives by the given procedure. This procedure should take as input a single node and return a boolean. The function can be arbitrary scheme functions. Any unquoted functions in the tree of the procedure argument will be applied to the given input node. This allows for recursive json:query invocations to filter based on specific properties of the given node.
               |  json-function-with-args             ; Invoke a json: function with the given arguments. These functions return a lambda that takes a node or vector of nodes as input.

json-function-with-args ::= (replace node)  ; json:replace  (Replace the input with the given node. This node object can contain unquoted functions, these functions will then be applied to the given input node (or vector). This function will allow creating new objects or vectors based on the given input.)
```

The given rules are transformed into functions that take a node or vector of nodes as argument. The final result is a function that can be applied to a parsed JSON object and transforms it using the given rules.

See `test_json_query.scm` for examples.
