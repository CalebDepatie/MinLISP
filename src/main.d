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

  int forloop(AST[] args) {
    int val = 0;

    for (int i = 0; i < args[0].execute(); i++)
      val += args[1].execute();

    return val;
  }

  int define(AST[] args) {
    const auto iden = cast(Iden) args[0];
    auto array = cast(Array) args[1];

    auto newfunc = delegate(AST[] childArgs) {
      // replace arguments
      void replaceExpr(Expression parentExpr, string replaceName, AST replaceVal) {

        for (int y = 0; y<parentExpr.args.length; y++) {
          if (const auto exprArg = cast(Iden) parentExpr.args[y]) {
            if (exprArg.getName() == replaceName) {
              parentExpr.args[y] = replaceVal;
            }
          } else if (auto childExpr = cast(Expression) parentExpr.args[y]) {
            replaceExpr(childExpr, replaceName, replaceVal);
          }
        }

        return;
      }

      if (auto expr = cast(Expression) args[2]) {
        for (int i = 0; i<array.length(); i++) {
          const auto currentArg = cast(Iden) array[i];
          replaceExpr(expr, currentArg.getName(), childArgs[i]);
        }

        return expr.execute();
      }


      return args[2].execute();
    };

    functions[iden.getName()] = Function(array.length(), newfunc);

    return 0;
  }

  int show(AST[] args) {
    return args[0].execute();
  }

  // basic maths
  functions["inc"] = Function(1, &increment);
  functions["dec"] = Function(1, &decrement);
  functions["add"] = Function(2, &add);
  functions["sub"] = Function(2, &sub);

  // control flow and helpers
  functions["forn"] = Function(2, &forloop);
  functions["show"] = Function(1, &show);
  functions["def"] = Function(3, &define);

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

Array parseArray(ref Token[] tokens)
in {
  assert(tokens[0].type == tokenType.lbracket);
} do {
  // drop start bracket
  tokens = tokens[1..$];
  AST[] values = [];

  while (tokens[0].type != tokenType.rbracket) {
    values ~= parseArg(tokens);
    //tokens = tokens[1..$];
  }

  // drop rbracken
  assert(tokens[0].type == tokenType.rbracket, "Ending array bracket not provided");
  tokens = tokens[1..$];

  return new Array(values);
}

AST parseArg(ref Token[] tokens)
in {
  assert(tokens[0].type != tokenType.rparen);
  } do {

  AST arg;
  switch (tokens[0].type) {
    case (tokenType.lparen): {
      arg = parseExpression(tokens);
      break;
    }
    case (tokenType.iden): {
      arg = new Iden(tokens[0].value);
      tokens = tokens[1..$];
      break;
    }
    case (tokenType.value): {
      arg = new Value(tokens[0].value);
      // drop cur arg
      tokens = tokens[1..$];
      break;
    }
    case (tokenType.lbracket): {
      arg = parseArray(tokens);
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
    string toString() const pure;
}

class Array : AST {
  private:
    AST[] values;

  public:
  this(AST[] values) @safe {
    this.values = values;
  }

  int execute() {
    return 0; // how do you convert an array of values to an int? you can't
  }

  int length() const pure @trusted {
    import std.conv: to;
    return to!int(values.length);
  }

  ref AST opIndex(int i) {
    return this.values[i];
  }

  override string toString() const pure @trusted {
    import std.conv: to;
    char[] s;

    s ~= "[";

    for (int i = 0; i < values.length; i++)
      s ~= " " ~ this.values[i].toString();

    s ~= " ]";

    return s;
  }
}

class Value : AST {
  private:
    immutable string value;

  public:
    this(string value) @safe {
      this.value = value;
    }

    int execute() {
      import std.conv: to;
      return to!int(this.value);
    }

    override string toString() const pure @trusted {
      import std.conv: to;

      return to!string(this.value);
    }
}

class Iden : Value {
  public:
    this(string value) @safe {
      super(value);
    }

    string getName() const pure @safe @nogc {
      return this.value;
    }
}

class Expression : AST {
  private:
    immutable string fnName;

  public:
    AST[] args;

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

    override string toString() const pure @trusted {
      char[] s;

      s ~= "( " ~ this.fnName;
      for (int i = 0; i < args.length; i++)
        s ~= " " ~ args[i].toString();

      s ~= " )";

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
