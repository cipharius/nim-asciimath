import unicode
import strutils
import asciimathTokens
import os

type
  NodeKind* = enum
    Expression
    Fragment
    Superscript
    Subscript
    Simple
    Token
  AMNode* = ref object
    case nKind: NodeKind
    of Token:
      token: AMToken
    else:
      discard
    next, prev: AMNode
    nextSibling, prevSibling: AMNode
    depth: int
  OptionalSub* = tuple
    node: AMNode
    token: AMToken
    rules: seq[AMNode]

const
  SubsTable = [Fragment, Superscript, Subscript, Simple, Token]

template exists(node: AMNode): bool = node != nil

proc skipWhitespace(str: string, pos: int = 0): int =
  ## Return position where whitespaces end
  result = pos
  while result < str.len:
    if not str.runeAt(result).isWhitespace:
      break
    else:
      inc result

proc tokenAt(str: string, pos: int = 0): AMToken =
  ## Return the matched token in `str` at `pos`
  let trailLen = str.len - pos

  # Try to find exact, maximal match
  var sym: AMToken
  var max_len = 0
  for token in AMTokens:
    let symbol = token.symbol
    if symbol.len <= trailLen and str.continuesWith(symbol, pos):
      if symbol.len > max_len:
        max_len = symbol.len
        sym = token

  if max_len > 0:
    return sym

  # Create a new CONST symbol
  if str[pos].isDigit:
    var number = ""
    for i in pos..<str.len:
      if str[i].isDigit:
        number &= str[i]
      else:
        break
    return (symbol:number, tex:number, tkKind:CONST)
  else:
    let ch = $str.runeAt(pos)
    return (symbol:ch, tex:ch, tkKind:CONST)

proc lexer*(str: string): seq[AMToken] =
  ## Convert `str` into asciimath tokens
  result = @[]

  var i = str.skipWhitespace()
  while i < str.len:
    let token = str.tokenAt(i)
    result.add(token)
    i = str.skipWhitespace(i + token.symbol.len)

proc `$`(node: AMNode): string =
  if not node.exists:
    return "nil"

  if node.nKind == Token:
    result = "Token[$#]" % node.token.symbol
  else:
    result = $node.nKind

  let
    prev = node.prevSibling
    next = node.nextSibling
  if prev.exists or next.exists:
    result &= "(prev: $#, next $#)" % [if prev.exists: $prev.nKind else: "nil",
                                       if next.exists: $next.nKind else: "nil"]

proc parser*(tokens: seq[AMToken]): AMNode =
  ## Generate parse tree from asciimath `tokens`.
  ##
  ## Grammar rules the parser follows specified in EBNF:
  ## 1. Expression ::= Fragment, {Fragment};
  ## 2. Fragment ::= Superscript, ["/", Superscript];
  ## 3. Superscript ::= Subscript, ["^", Simple];
  ## 4. Subscript ::= Simple, ["_", Simple];
  ## 5. Simple ::= constant | LeftBracket, Expression, RightBracket |
  ##               Unary, Simple | Binary, Simple, Simple;
  ##
  ## Note that this grammar doesn't represent asciimath language, but
  ## rather serves as blueprint for the parser's logic. Redudant nodes
  ## such as superscript without sibling nodes will get automatically
  ## colapsed.
  var
    nodes: seq[AMNode] = @[]
    symbols: seq[AMNode] = @[AMNode(nKind: Expression, depth: 0)]
    optional: seq[OptionalSub] = @[]
    curToken = 0
    noSub = false
  result = symbols[0]

  while curToken < tokens.len:
    let token = tokens[curToken]
    noSub = false

    if symbols.len > 0:
      case symbols[^1].nKind
      of Expression:
        let depth = symbols[^1].depth + 1
        nodes.add(symbols.pop())
        symbols.add(AMNode(nKind: Fragment, depth: depth))
      of Fragment:
        let depth = symbols[^1].depth + 1
        var node = AMNode(nKind: Superscript, depth: depth)
        let
          optToken = toToken("/")
          rules: seq[AMNode] = @[AMNode(nKind: Token, depth: depth, token: optToken),
                                 AMNode(nKind: Superscript, depth: depth)]
        nodes.add(symbols.pop())
        symbols.add(node)
        optional.add((node: node, token: optToken, rules: rules))
      of Superscript:
        let depth = symbols[^1].depth + 1
        var node = AMNode(nKind: Subscript, depth: depth)
        let
          optToken = toToken("^")
          rules: seq[AMNode] = @[AMNode(nKind: Token, depth: depth, token: optToken), AMNode(nKind: Simple, depth: depth)]

        nodes.add(symbols.pop())
        symbols.add(node)
        optional.add((node: node, token: optToken, rules: rules))
      of Subscript:
        let depth = symbols[^1].depth
        var node = AMNode(nKind: Simple, depth: depth+1)
        let
          optToken = toToken("_")
          rules: seq[AMNode] = @[AMNode(nKind: Token, depth: depth, token: optToken),
                                 AMNode(nKind: Simple, depth: depth)]

        nodes.add(symbols.pop())
        symbols.add(node)
        optional.add((node: node, token: optToken, rules: rules))
      of Simple:
        let depth = symbols[^1].depth + 1
        nodes.add(symbols.pop())

        case token.tkKind
        of UNARY:
          symbols.add(AMNode(nKind: Simple, depth: depth))
          symbols.add(AMNode(nKind: Token, depth: depth, token: token))

          symbols[^1].nextSibling = symbols[^2]
          symbols[^2].prevSibling = symbols[^1]
        of BINARY:
          symbols.add(AMNode(nKind: Simple, depth: depth))
          symbols.add(AMNode(nKind: Simple, depth: depth))
          symbols.add(AMNode(nKind: Token, depth: depth, token: token))

          symbols[^1].nextSibling = symbols[^2]
          symbols[^2].nextSibling = symbols[^3]
          symbols[^3].prevSibling = symbols[^2]
          symbols[^2].prevSibling = symbols[^1]
        of LEFTBRACKET:
          symbols.add(AMNode(nKind: Token, depth: depth, token: (symbol: "", tex: "", tkKind: RIGHTBRACKET)))
          symbols.add(AMNode(nKind: Expression, depth: depth))
          symbols.add(AMNode(nKind: Token, depth: depth, token: token))

          symbols[^1].nextSibling = symbols[^2]
          symbols[^2].nextSibling = symbols[^3]
          symbols[^3].prevSibling = symbols[^2]
          symbols[^2].prevSibling = symbols[^1]
        else:
          symbols.add(AMNode(nKind: Token, depth: depth, token: token))
      of Token:
        let
          symbol = symbols[^1]
          isRightBracket = symbol.token.tkKind == RIGHTBRACKET
        if symbol.token == token or (isRightBracket and token.tkKind == RIGHTBRACKET):
          var node = symbols.pop()

          if node.token.tkKind == RIGHTBRACKET:
            node.token = token

          nodes.add(node)
          curToken.inc()
        else:
          noSub = true
    else:
      # No symbols in stack
      noSub = true

    if noSub:
      if optional.len == 0:
        var depth = 0
        for i in 1..<symbols.len:
          if symbols[^i].nKind == Expression:
            depth = symbols[^i].depth + 1
            break
        symbols.add(AMNode(nKind: Fragment, depth: depth))
      else:
        while optional.len > 0:
          var optRule = optional.pop()
          if optRule.token == token:
            var prev: AMNode = nil

            for i, rule in optRule.rules:
              if i == 0:
                rule.prevSibling = optRule.node
                optRule.node.nextSibling = rule
              else:
                rule.prevSibling = prev
                prev.nextSibling = rule
              prev = rule

            while optRule.rules.len > 0:
              symbols.add(optRule.rules.pop())
            break

  # Collapse tree and create references
  var prev: AMNode = nil
  var collapsed: int = 0
  for node in nodes:
    if prev != nil:
      case node.nKind
      of Superscript, Subscript, Simple:
        if node.nextSibling != nil or node.prevSibling != nil:
          node.prev = prev
          prev.next = node
          prev = node
        else:
          collapsed.inc()
      else:
        node.prev = prev
        prev.next = node
        prev = node
    else:
      prev = node

iterator items(node: AMNode): AMNode =
  var node = node
  while node.next != nil:
    node = node.next
    yield node


proc toLatex*(expression: AMNode): string =
  var prevDepth = 0
  var openBrackets: seq[int] = @[]
  result = ""

  for node in expression:
    if prevDepth > node.depth and openBrackets.len > 0 and
       openBrackets[^1] >= node.depth:
      discard openBrackets.pop()
      result &= "}"

    case node.nKind
    of Superscript:
      if node.nextSibling.exists:
        result &= r"\frac{"
        openBrackets.add(node.depth)
      elif node.prevSibling.exists:
        result &= "{"
        openBrackets.add(node.depth)
    # of Subscript:
    #   if node.nextSibling.exists and node.next.nKind != Token:
    #     result &= "{"
    #     openBrackets.add(node.depth)
    of Token:
      if node.token != "/".toToken():
        result &= node.token.tex
    else:
      discard

    prevDepth = node.depth

  if openBrackets.len > 0:
    result &= "}".repeat(openBrackets.len)

proc treeRepr*(expression: AMNode): string =
  result = ""
  for node in expression:
    if node.depth > 1:
      result &= "  ".repeat(node.depth - 1)
    result &= $node
    result &= "\n"

when isMainModule:
  let tokens = lexer(paramStr(1))
  let tree = parser(tokens)
  echo tree.toLatex()
  echo tree.treeRepr()
