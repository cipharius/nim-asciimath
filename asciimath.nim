import unicode, strutils, os
import asciimath.asciimathTokens

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
  OptionalSubKind = enum
    Optional
    Repeatition
  OptionalSub* = object
    case kind: OptionalSubKind
    of Optional:
      token: AMToken
      rules: seq[AMNode]
    else:
      discard
    node: AMNode

# AMNode procedures
template exists*(node: AMNode): bool = node != nil
iterator items*(node: AMNode): AMNode =
  var node = node
  while node.next != nil:
    node = node.next
    yield node

iterator `^`(nodes: openArray[AMNode]): AMNode =
  for i in 1..nodes.len:
    yield nodes[^i]

proc `==`*(node: AMNode, kind: NodeKind): bool {.inline.} =
  node.exists and node.nKind == kind

proc contains*(arr: set[NodeKind], node: AMNode): bool =
  result = false
  for kind in arr:
    if node == kind:
      return true

converter toNode(kind: NodeKind): AMNode = AMNode(nKind: kind)
converter toNode(str: string): AMNode = AMNode(nKind: Token, token: str.toToken())
converter toNode(token: AMToken): AMNode = AMNode(nKind: Token, token: token)

template addSymbols(symbolStack: seq[AMNode], depth: int,
                    symbols: varargs[AMNode, toNode]): untyped =
  for symbol in ^symbols:
    symbol.depth = depth
    symbolStack.add(symbol)
  for i in 1..symbols.len:
    if i < symbols.len:
      symbolStack[^i].nextSibling = symbolStack[^(i+1)]
    if i > 1:
      symbolStack[^i].prevSibling = symbolStack[^(i-1)]


# OptionalSub procedure
template addOptional(ruleStack: seq[OptionalSub], forNode: AMNode,
                     ruleTokens: varargs[AMNode, toNode]): untyped =
  let rules = @ruleTokens
  for rule in rules:
    rule.depth = forNode.depth
  ruleStack.add(OptionalSub(kind: Optional, node: forNode,
                            token: ruleTokens[0].token, rules: rules))

template addRepeat(ruleStack: seq[OptionalSub], forNode: AMNode): untyped =
  ruleStack.add(OptionalSub(kind: Repeatition, node: forNode))

# Parser procedures
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

  # Create dynamic symbol
  if str[pos] == '"':
    var endPos = 0
    for i in (pos+1)..<str.len:
      if str[i] == '"':
        endPos = i
        break
    if endPos > pos:
      let text = str[pos..endPos]
      return (symbol:text, tex:r"\text{$#}" % text[1..^2], tkKind:TEXT)
    else:
      return (symbol:($str[pos]), tex:($str[pos]), tkKind:CONST)
  elif str[pos].isDigit:
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

  result &= ": " & $node.depth

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
      let depth = symbols[^1].depth + 1

      case symbols[^1].nKind
      of Expression:
        let symbol = symbols.pop()
        nodes.add(symbol)
        symbols.addSymbols(depth, Fragment)
        optional.addRepeat(symbol)
      of Fragment:
        var node = AMNode(nKind: Superscript, depth: depth)
        nodes.add(symbols.pop())
        symbols.add(node)
        optional.addOptional(node, "/", Superscript)
      of Superscript:
        var node = AMNode(nKind: Subscript, depth: depth)

        nodes.add(symbols.pop())
        symbols.add(node)
        optional.addOptional(node, "^", Simple)
      of Subscript:
        var node = AMNode(nKind: Simple, depth: depth)
        nodes.add(symbols.pop())
        symbols.add(node)
        optional.addOptional(node, "_", Simple)
      of Simple:
        nodes.add(symbols.pop())

        case token.tkKind
        of UNARY:
          symbols.addSymbols(depth, token, Simple)
        of BINARY:
          symbols.addSymbols(depth, token, Simple, Simple)
        of LEFTBRACKET:
          symbols.addSymbols(depth, token, Expression,
                             (symbol:"", tex:"", tkKind:RIGHTBRACKET))
        else:
          symbols.addSymbols(depth, token)
      of Token:
        let
          symbol = symbols[^1]
          isRightBracket = symbol.token.tkKind == RIGHTBRACKET
        if symbol.token == token or (isRightBracket and token.tkKind == RIGHTBRACKET):
          var node = symbols.pop()

          if node.token.tkKind == RIGHTBRACKET:
            node.token = token
            while optional[^1].kind != Repeatition:
              discard optional.pop()
            discard optional.pop()

          nodes.add(node)
          curToken.inc()
        else:
          noSub = true

      if not noSub and nodes.len > 1:
        nodes[^1].prev = nodes[^2]
        nodes[^2].next = nodes[^1]
    else:
      noSub = true

    if noSub:
      if optional[^1].kind == Repeatition:
        let repeat = optional[^1]
        var newFrag = AMNode(nKind: Fragment)
        newFrag.depth = repeat.node.depth + 1

        var node: AMNode = repeat.node.next
        while node.nextSibling != nil:
          node = node.nextSibling

        newFrag.prevSibling = node
        node.nextSibling = newFrag
        symbols.add(newFrag)
      else:
        while optional[^1].kind != Repeatition:
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
  var lastDepth = 0
  var deleteList: seq[int] = @[]
  for i, node in nodes:
    if prev != nil:
      case node.nKind
      of Superscript, Subscript, Simple:
        if not (node.nextSibling.exists or node.prevSibling.exists):
          collapsed.inc()
          lastDepth = node.depth
          deleteList.add(i)
          continue
      else:
        discard

      node.prev = prev
      prev.next = node

      if node.prevSibling.exists:
        node.depth = node.prevSibling.depth
      elif node.depth > node.prev.depth:
        node.depth = node.prev.depth + 1

    prev = node
    lastDepth = node.depth

  for i in 0..<deleteList.len:
    nodes.delete(deleteList[i] - i)

proc toLatex*(expression: AMNode): string =
  ## Generate LaTeX expression from AMNode tree
  var openBrackets: seq[int] = @[]
  result = ""

  for node in expression:
    if openBrackets.len > 0 and node.depth <= openBrackets[^1]:
      discard openBrackets.pop()
      result &= '}'

    case node.nKind
    of Superscript:
      if node.nextSibling.exists:
        result &= r"\frac{ "
        openBrackets.add(node.depth)
      elif node.prevSibling.exists:
        result &= "{ "
        openBrackets.add(node.depth)
    of Simple:
      # Check if node is unary or binary argument
      let prevSibling = node.prevSibling
      if prevSibling.exists and (prevSibling == Token or prevSibling.prevSibling == Token):
        result &= "{ "
        openBrackets.add(node.depth)
    of Token:
      if node.token != "/".toToken():
        case node.token.tkKind
        of LEFTBRACKET:
          if node.prev == Simple or node.prev == Superscript:
            result &= "{ "
            openBrackets.add(node.depth)
        of RIGHTBRACKET:
          if openBrackets.len > 0 and node.depth == openBrackets[^1]:
            discard openBrackets.pop()
            result &= '}'
        else:
          var token = node.token

          if token.symbol == "^" and node.prev == Fragment:
            # Handle invalid `^` token nicely
            token.tex = r"\text{\textasciicircum}"

          result &= token.tex & ' '
    else:
      discard

  if openBrackets.len > 0:
    result &= '}'.repeat(openBrackets.len)

template toLatex*(str: string): string =
  ## Translate asciimath `str` to LaTeX
  str.lexer().parser().toLaTeX()

proc treeRepr*(expression: AMNode): string =
  ## Print AMNode tree
  result = ""
  for node in expression:
    if node.depth > 1:
      result &= "  ".repeat(node.depth - 1)
    result &= $node
    result &= "\n"
