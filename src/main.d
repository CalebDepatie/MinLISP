module main;

int main(string[] args) {
  import std.stdio: writeln;
  import std.file: readText;

  if (args.length == 1) {
    writeln("No file provided");
    return -1;
  }

  // get file name
  string fileName = args[1];

  string fileText;
  try {
    fileText = readText(fileName);
  } catch (Throwable) {
    writeln("Could not load file: " ~ fileName);
    writeln("Please ensure file exists and path is correct.");
    return -1;
  }

  // load built ins
  int increment(AST[] args) {
    return args[0].execute() + 1;
  }

  int decrement(AST[] args) {
    return args[0].execute() - 1;
  }

  int add(AST[] args) {
    return args[0].execute() + args[1].execute();
  }

  int sub(AST[] args) {
    return args[0].execute() - args[1].execute();
  }

  functions["inc"] = Function(1, &increment);
  functions["dec"] = Function(1, &decrement);

  functions["add"] = Function(2, &add);
  functions["sub"] = Function(2, &sub);

  auto lex = new Lexer(fileText);

  auto tokens = lex.getTokens();

  debug {
    writeln("Lexed Tokens:");
    writeln(tokens);
  }

  auto tree = parse(tokens);

  debug {
    writeln("\nParsed AST:");
    writeln(tree);
  }

  debug {
    writeln("\nExecution Result:");
  }

  foreach(node; tree) {
    writeln(node.execute());
  }

  return 0;
}

AST[] parse(ref Token[] tokens)
 in {
    assert (tokens.length > 0);
    assert (tokens[0].type == tokenType.lparen);
  } out (res) {
    assert (res.length > 0);
  } do {
  AST[] tree;

  if (tokens[0].type == tokenType.lparen) {
    tree ~= parseExpression(tokens);
  }

  if (tokens.length == 0) {
    return tree;
  }

  return tree ~ parse(tokens);
}

Expression parseExpression(ref Token[] tokens)
  in {
    assert (tokens[0].type == tokenType.lparen);
  } do {

  // drop L paren
  tokens = tokens[1..$];

  // get fn name to call
  assert (tokens[0].type == tokenType.iden);
  string name = tokens[0].value;
  tokens = tokens[1..$];

  // setup arguments
  AST[] args = [];

  while (tokens[0].type != tokenType.rparen) {
    args ~= parseArg(tokens);
  }

  // drop rparen
  assert(tokens[0].type == tokenType.rparen, "Expected expression end, received " ~ tokens[0].toString());
  tokens = tokens[1..$];

  return new Expression(name, args);
}

AST parseArg(ref Token[] tokens)
in {
  assert(tokens[0].type != tokenType.rparen);
  } do {
  import std.conv: to;

  AST arg;
  switch (tokens[0].type) {
    case (tokenType.lparen): {
      arg = parseExpression(tokens);
      break;
    }
    case (tokenType.value): {
      arg = new Value(to!int(tokens[0].value));
      // drop cur arg
      tokens = tokens[1..$];
      break;
    }
    default: assert(0);
  }

  return arg;
}

Function[string] functions;

struct Function {
  int argCount;

  int delegate(AST[]) func;
}

interface AST {
  public:
    int execute();
    string toString() pure;
}

class Value : AST {
  private:
    immutable int value;

  public:
    this(int value) @safe {
      this.value = value;
    }

    int execute() {
      return this.value;
    }

    override string toString() pure @trusted {
      import std.conv: to;

      return to!string(this.value);
    }
}

class Expression : AST {
  private:
    immutable string fnName;
    AST[] args;

  public:
    this(string fnName, AST[] args) @safe {
      this.fnName = fnName;
      this.args = args;
    }

    int execute() {
      auto fnp = this.fnName in functions;
      assert(fnp !is null, "Function " ~ this.fnName ~ " does not exist");

      auto fn = functions[this.fnName];
      assert(args.length == fn.argCount, "Incorrect number of arguments provided to function");

      return fn.func(args);
    }

    override string toString() pure @trusted {
      char[] s;

      s ~= "(" ~ this.fnName ~ " ";
      for (int i = 0; i < args.length; i++)
        s ~= args[i].toString();

      s ~= ")";

      return s;
    }
}

class Lexer {
  private:
    immutable string fileText;
    int curChar;

    bool isEmpty() pure @safe @nogc {
      return curChar == fileText.length;
    }

    Token getNext() pure @safe {
      import std.conv: to;
      import std.ascii: isAlpha, isDigit;
    lexStart:
      string curVal;
      curVal = to!string(fileText[curChar++]);
      tokenType tType;

      // skip comment lines
      if (curVal == ";") {
        while(curVal != "\n")
          curVal = to!string(fileText[curChar++]);
      }

      // skip tokens we don't care about
      if (curVal == "\n" || curVal == "\r" || curVal == " " || curVal == ",") {
        if (this.isEmpty())
          return null;
        goto lexStart;
      }

      switch (curVal) {
        case "(": {
          tType = tokenType.lparen;
          break;
        }
        case ")": {
          tType = tokenType.rparen;
          break;
        }
        case "[": {
          tType = tokenType.lbracket;
          break;
        }
        case "]": {
          tType = tokenType.rbracket;
          break;
        }
        default: tType = tokenType.unknown;
      }

      // check if value or iden
      if (isAlpha(curVal[$-1])) {
        while (isAlpha(fileText[curChar]))
          curVal ~= to!string(fileText[curChar++]);

        tType = tokenType.iden;
      }

      if (isDigit(curVal[$-1])) {
        while (isDigit(fileText[curChar]))
          curVal ~= to!string(fileText[curChar++]);

        tType = tokenType.value;
      }

      return new Token(tType, curVal);
    }

  public:
    this(string text) @safe {
      this.fileText = text;
      this.curChar = 0;
    }

    Token[] getTokens() @safe {
      Token[] tokens = [];
      while (true) {
        auto newToken = this.getNext();

        if (newToken !is null)
          tokens ~= newToken;
        else
          break;
      }

      return tokens;
    }

}

enum tokenType {
  unknown,
  lparen,
  rparen,
  lbracket,
  rbracket,
  iden,
  value
}

class Token {
  immutable tokenType type;
  immutable string value;

  this(tokenType type, string value) pure @safe @nogc {
    this.type = type; this.value = value;
  }

  this(tokenType type, char value) pure @safe {
    import std.conv: to;
    this.type = type; this.value = to!string(value);
  }

  override string toString() pure @trusted {
    import std.conv: to;
    char[] s;

    s ~= to!string(this.type) ~ ' ';
    s ~= this.value;

    return s;
  }
}
