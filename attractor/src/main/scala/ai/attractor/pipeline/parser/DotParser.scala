package ai.attractor.pipeline.parser

// ---------------------------------------------------------------------------
// Recursive-descent DOT parser for the pipeline graph subset.
//
// Strategy:
//   1. Strip comments (// line and /* block */)
//   2. Tokenize into a flat token list
//   3. Parse the token stream with a recursive-descent parser
// ---------------------------------------------------------------------------

object DotParser:

  def parse(input: String): Either[String, Graph] =
    for
      stripped  <- Right(stripComments(input))
      tokens   <- tokenize(stripped)
      graph    <- Parser(tokens).parseGraph()
    yield graph

  // -------------------------------------------------------------------------
  // Phase 1: Comment stripping
  // -------------------------------------------------------------------------

  private def stripComments(src: String): String =
    val sb = StringBuilder()
    var i = 0
    var inString = false
    while i < src.length do
      if inString then
        if src(i) == '\\' && i + 1 < src.length then
          sb.append(src(i))
          sb.append(src(i + 1))
          i += 2
        else
          if src(i) == '"' then inString = false
          sb.append(src(i))
          i += 1
      else if src(i) == '"' then
        inString = true
        sb.append(src(i))
        i += 1
      else if i + 1 < src.length && src(i) == '/' && src(i + 1) == '/' then
        // line comment - skip to end of line
        i += 2
        while i < src.length && src(i) != '\n' do i += 1
      else if i + 1 < src.length && src(i) == '/' && src(i + 1) == '*' then
        // block comment - skip to closing */
        i += 2
        while i + 1 < src.length && !(src(i) == '*' && src(i + 1) == '/') do i += 1
        if i + 1 < src.length then i += 2 // skip */
      else
        sb.append(src(i))
        i += 1
    sb.toString

  // -------------------------------------------------------------------------
  // Phase 2: Tokenization
  // -------------------------------------------------------------------------

  private enum Token:
    case Ident(value: String)
    case QuotedString(value: String)
    case Arrow           // ->
    case LBrace          // {
    case RBrace          // }
    case LBracket        // [
    case RBracket        // ]
    case Equals          // =
    case Semicolon       // ;
    case Comma           // ,

  private def tokenize(src: String): Either[String, List[Token]] =
    val tokens = scala.collection.mutable.ListBuffer[Token]()
    var i = 0

    def skipWhitespace(): Unit =
      while i < src.length && src(i).isWhitespace do i += 1

    def readQuotedString(): Either[String, String] =
      val sb = StringBuilder()
      i += 1 // skip opening "
      while i < src.length && src(i) != '"' do
        if src(i) == '\\' && i + 1 < src.length then
          val next = src(i + 1)
          next match
            case 'n'  => sb.append('\n')
            case 't'  => sb.append('\t')
            case '\\' => sb.append('\\')
            case '"'  => sb.append('"')
            case _    => sb.append('\\'); sb.append(next)
          i += 2
        else
          sb.append(src(i))
          i += 1
      if i >= src.length then Left("Unterminated string literal")
      else
        i += 1 // skip closing "
        Right(sb.toString)

    def readIdent(): String =
      val start = i
      while i < src.length && (src(i).isLetterOrDigit || src(i) == '_' || src(i) == '.') do
        i += 1
      src.substring(start, i)

    var error: Option[String] = None
    while i < src.length && error.isEmpty do
      skipWhitespace()
      if i >= src.length then () // done
      else
        src(i) match
          case '{' => tokens += Token.LBrace; i += 1
          case '}' => tokens += Token.RBrace; i += 1
          case '[' => tokens += Token.LBracket; i += 1
          case ']' => tokens += Token.RBracket; i += 1
          case '=' => tokens += Token.Equals; i += 1
          case ';' => tokens += Token.Semicolon; i += 1
          case ',' => tokens += Token.Comma; i += 1
          case '-' =>
            if i + 1 < src.length && src(i + 1) == '>' then
              tokens += Token.Arrow
              i += 2
            else
              // Bare hyphen is part of an identifier (e.g. node-name)
              val start = i
              i += 1
              while i < src.length && (src(i).isLetterOrDigit || src(i) == '_' || src(i) == '-' || src(i) == '.') do
                i += 1
              tokens += Token.Ident(src.substring(start, i))
          case '"' =>
            readQuotedString() match
              case Right(s) => tokens += Token.QuotedString(s)
              case Left(e)  => error = Some(e)
          case c if c.isLetter || c == '_' =>
            val id = readIdent()
            tokens += Token.Ident(id)
          case c if c.isDigit =>
            val id = readIdent()
            tokens += Token.Ident(id)
          case other =>
            error = Some(s"Unexpected character: '$other' at position $i")

    error match
      case Some(e) => Left(e)
      case None    => Right(tokens.toList)

  // -------------------------------------------------------------------------
  // Phase 3: Recursive-descent parser
  // -------------------------------------------------------------------------

  private class Parser(tokens: List[Token]):
    private var pos = 0

    private def peek: Option[Token] =
      if pos < tokens.length then Some(tokens(pos)) else None

    private def advance(): Token =
      val t = tokens(pos)
      pos += 1
      t

    private def expect(pred: Token => Boolean, desc: String): Either[String, Token] =
      peek match
        case Some(t) if pred(t) => Right(advance())
        case Some(t) => Left(s"Expected $desc but got $t at token position $pos")
        case None    => Left(s"Expected $desc but reached end of input")

    private def expectIdent(desc: String): Either[String, String] =
      expect(
        { case Token.Ident(_) | Token.QuotedString(_) => true; case _ => false },
        desc
      ).map:
        case Token.Ident(v)        => v
        case Token.QuotedString(v) => v
        case _                     => "" // unreachable

    private def skipSemicolons(): Unit =
      while peek.contains(Token.Semicolon) do advance()

    private def parseAttributeValue(): Either[String, String] =
      peek match
        case Some(Token.QuotedString(_)) =>
          val Token.QuotedString(v) = advance(): @unchecked
          Right(v)
        case Some(Token.Ident(_)) =>
          val Token.Ident(v) = advance(): @unchecked
          Right(v)
        case other =>
          Left(s"Expected attribute value but got $other at token position $pos")

    /** Parse [...] attribute list, returning a Map. */
    private def parseAttrList(): Either[String, Map[String, String]] =
      if !peek.contains(Token.LBracket) then Right(Map.empty)
      else
        advance() // consume [
        val attrs = scala.collection.mutable.Map[String, String]()
        var done = false
        var error: Option[String] = None

        while !done && error.isEmpty do
          peek match
            case Some(Token.RBracket) =>
              advance()
              done = true
            case None =>
              error = Some("Unterminated attribute list, expected ]")
            case _ =>
              val result = for
                key <- expectIdent("attribute key")
                _   <- expect(_ == Token.Equals, "'='")
                v   <- parseAttributeValue()
              yield (key, v)

              result match
                case Right((k, v)) =>
                  attrs(k) = v
                  // optional comma or semicolon separator
                  peek match
                    case Some(Token.Comma)     => advance()
                    case Some(Token.Semicolon) => advance()
                    case _                     => ()
                case Left(e) => error = Some(e)

        error match
          case Some(e) => Left(e)
          case None    => Right(attrs.toMap)

    /** Parse the top-level `digraph Name { ... }`. */
    def parseGraph(): Either[String, Graph] =
      for
        _ <- expect(
          { case Token.Ident(v) => v.toLowerCase == "digraph"; case _ => false },
          "'digraph'"
        )
        name <- expectIdent("graph name")
        _    <- expect(_ == Token.LBrace, "'{'")
        result <- parseBody(
          graphAttrs = Map.empty,
          nodeDefaults = Map.empty,
          edgeDefaults = Map.empty,
          nodes = Map.empty,
          edges = List.empty
        )
        _ <- expect(_ == Token.RBrace, "'}'")
      yield
        val (ga, nd, ed, ns, es) = result
        Graph(name, ns, es, ga)

    /** Parse the body of a digraph or subgraph block. */
    private def parseBody(
        graphAttrs: Map[String, String],
        nodeDefaults: Map[String, String],
        edgeDefaults: Map[String, String],
        nodes: Map[String, Node],
        edges: List[Edge]
    ): Either[String, (Map[String, String], Map[String, String], Map[String, String], Map[String, Node], List[Edge])] =
      var ga = graphAttrs
      var nd = nodeDefaults
      var ed = edgeDefaults
      var ns = nodes
      var es = edges
      var error: Option[String] = None

      while peek.isDefined && !peek.contains(Token.RBrace) && error.isEmpty do
        skipSemicolons()
        if peek.contains(Token.RBrace) then () // break
        else
          val result = parseStatement(ga, nd, ed, ns, es)
          result match
            case Right((ga2, nd2, ed2, ns2, es2)) =>
              ga = ga2; nd = nd2; ed = ed2; ns = ns2; es = es2
              skipSemicolons()
            case Left(e) => error = Some(e)

      error match
        case Some(e) => Left(e)
        case None    => Right((ga, nd, ed, ns, es))

    /** Parse a single statement within a graph body. */
    private def parseStatement(
        ga: Map[String, String],
        nd: Map[String, String],
        ed: Map[String, String],
        ns: Map[String, Node],
        es: List[Edge]
    ): Either[String, (Map[String, String], Map[String, String], Map[String, String], Map[String, Node], List[Edge])] =
      peek match
        // graph [...]
        case Some(Token.Ident(v)) if v.toLowerCase == "graph" =>
          advance()
          parseAttrList().map: attrs =>
            (ga ++ attrs, nd, ed, ns, es)

        // node [...]
        case Some(Token.Ident(v)) if v.toLowerCase == "node" =>
          advance()
          parseAttrList().map: attrs =>
            (ga, nd ++ attrs, ed, ns, es)

        // edge [...]
        case Some(Token.Ident(v)) if v.toLowerCase == "edge" =>
          advance()
          parseAttrList().map: attrs =>
            (ga, nd, ed ++ attrs, ns, es)

        // subgraph name { ... }
        case Some(Token.Ident(v)) if v.toLowerCase == "subgraph" =>
          advance()
          // optional subgraph name
          val subName = peek match
            case Some(Token.Ident(_)) | Some(Token.QuotedString(_)) =>
              val n = advance() match
                case Token.Ident(s)        => s
                case Token.QuotedString(s) => s
                case _                     => ""
              Some(n)
            case _ => None
          for
            _      <- expect(_ == Token.LBrace, "'{'")
            result <- parseBody(ga, nd, ed, ns, es)
            _      <- expect(_ == Token.RBrace, "'}'")
          yield result

        // node or edge statement starting with an identifier
        case Some(Token.Ident(_)) | Some(Token.QuotedString(_)) =>
          parseNodeOrEdge(ga, nd, ed, ns, es)

        case Some(other) =>
          Left(s"Unexpected token: $other at position $pos")

        case None =>
          Left("Unexpected end of input")

    /** Parse a node statement, edge chain, or standalone graph attribute declaration. */
    private def parseNodeOrEdge(
        ga: Map[String, String],
        nd: Map[String, String],
        ed: Map[String, String],
        ns: Map[String, Node],
        es: List[Edge]
    ): Either[String, (Map[String, String], Map[String, String], Map[String, String], Map[String, Node], List[Edge])] =
      for
        firstId <- expectIdent("node id")
        result <- peek match
          // Edge chain: id -> id [-> id ...] [attrs]
          case Some(Token.Arrow) =>
            parseEdgeChain(firstId, ga, nd, ed, ns, es)
          // Standalone graph attribute: key = value (e.g. goal = "Build feature")
          case Some(Token.Equals) =>
            advance() // consume =
            parseAttributeValue().map: value =>
              (ga + (firstId -> value), nd, ed, ns, es)
          // Node statement: id [attrs]
          case _ =>
            parseAttrList().map: attrs =>
              val merged = nd ++ attrs
              val node = ns.get(firstId) match
                case Some(existing) =>
                  existing.copy(attributes = existing.attributes ++ merged)
                case None =>
                  Node(firstId, merged)
              (ga, nd, ed, ns + (firstId -> node), es)
      yield result

    /** Parse an edge chain: already consumed first id, expects -> id [-> id ...] [attrs] */
    private def parseEdgeChain(
        firstId: String,
        ga: Map[String, String],
        nd: Map[String, String],
        ed: Map[String, String],
        ns: Map[String, Node],
        es: List[Edge]
    ): Either[String, (Map[String, String], Map[String, String], Map[String, String], Map[String, Node], List[Edge])] =
      val chain = scala.collection.mutable.ListBuffer[String](firstId)
      var error: Option[String] = None

      while peek.contains(Token.Arrow) && error.isEmpty do
        advance() // consume ->
        expectIdent("edge target") match
          case Right(id) => chain += id
          case Left(e)   => error = Some(e)

      error match
        case Some(e) => Left(e)
        case None =>
          parseAttrList().map: attrs =>
            val mergedEdgeAttrs = ed ++ attrs
            // Ensure all referenced nodes exist (create with defaults if not)
            var updatedNodes = ns
            for id <- chain.toList do
              if !updatedNodes.contains(id) then
                updatedNodes = updatedNodes + (id -> Node(id, nd))

            val newEdges = chain.toList.sliding(2).collect:
              case List(a, b) => Edge(a, b, mergedEdgeAttrs)
            .toList

            (ga, nd, ed, updatedNodes, es ++ newEdges)
