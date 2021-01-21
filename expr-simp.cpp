#include <string>
#include <sstream>
#include <vector>
#include <iostream>
#include <stack>

enum TokenType {
  L_TOKEN_PUNCTUATION,
  L_TOKEN_SYMBOL,
  L_TOKEN_INTEGER,
  L_TOKEN_NULL,
};

struct Token {
  TokenType ty;
  char punctuation;
  std::string symbol;
  int integer;
};

constexpr bool is_valid_punctuation(char c) {
  return c == '(' || c == ')' || c == '+' || c == '*' || c == '/' || c == '%';
}
std::vector<Token> lex(const char* expr) {
  if (expr == nullptr) { return {}; }

  std::vector<Token> rv;

  rv.push_back(Token { L_TOKEN_PUNCTUATION, '(' });

  TokenType last_ty = L_TOKEN_PUNCTUATION;
  const char* beg = expr;
  const char* pos = expr;

  for (;;) {
    const char c = *pos;

    bool is_digit = c >= '0' && c <= '9';
    bool is_alphabet = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    bool is_whitespace = c == ' ' || c == '\t';

    TokenType ty;
    switch (last_ty) {
    case L_TOKEN_PUNCTUATION:
      if (is_digit) {
        ty = L_TOKEN_INTEGER;
      } else if (is_alphabet) {
        ty = L_TOKEN_SYMBOL;
      } else {
        ty = L_TOKEN_PUNCTUATION;
      }
      break;
    case L_TOKEN_INTEGER:
      if (is_digit) {
        ty = L_TOKEN_INTEGER;
      } else if (is_alphabet) {
        throw std::logic_error("numbers cannot preceed alphabets");
      } else {
        ty = L_TOKEN_PUNCTUATION;
      }
      break;
    case L_TOKEN_SYMBOL:
      if (is_digit || is_alphabet) {
        ty = L_TOKEN_SYMBOL;
      } else {
        ty = L_TOKEN_PUNCTUATION;
      }
      break;
    }

    if (beg != pos && (ty == L_TOKEN_PUNCTUATION || ty != last_ty)) {
      std::string token_lit(beg, pos);
      beg = pos;

      if (token_lit[0] != ' ' || token_lit[0] != ' ') {
        Token token {};
        token.ty = last_ty;
        switch (last_ty) {
        case L_TOKEN_PUNCTUATION:
          if (token_lit.size() != 1) {
            throw std::logic_error("punctuation must have length of 1");
          }
          if (!is_valid_punctuation(token_lit[0])) {
            throw std::runtime_error("unrecognized punctuation");
          }
          token.punctuation = token_lit[0];
          break;
        case L_TOKEN_INTEGER:
          token.integer = std::stoi(token_lit);
          break;
        case L_TOKEN_SYMBOL:
          token.symbol = token_lit;
          break;
        }

        rv.push_back(token);
      }
    }

    if (c == '\0') { break; }

    last_ty = ty;
    ++pos;
  }

  rv.push_back(Token { L_TOKEN_PUNCTUATION, ')' });

  return rv;
}


class Tokenizer {
  std::vector<Token> tokens_;
  size_t i_;

public:
  Tokenizer(const std::string& lit) : tokens_(lex(lit.c_str())), i_() {
  }

  inline const bool empty() const {
    return tokens_.size() <= i_;
  }
  inline const Token& peak() const {
    if (empty()) {
      throw std::logic_error("tokenizer is empty");
    }
    return tokens_[i_];
  }
  inline Token next() {
    if (empty()) {
      throw std::logic_error("tokenizer is empty");
    }
    return std::move(tokens_[i_++]);
  }

  inline bool next_is_symbol() const {
    return peak().ty == L_TOKEN_SYMBOL;
  }
  inline bool next_is_integer() const {
    return peak().ty == L_TOKEN_INTEGER;
  }
  inline bool next_is_punctuation(char punc = '\0') const {
    return peak().ty == L_TOKEN_PUNCTUATION &&
      (punc == '\0' || peak().punctuation == punc);
  }
};


enum AstNodeType {
  L_AST_CONSTANT,
  L_AST_SYMBOL,
  L_AST_SUBNODE,
};

struct Ast {
  AstNodeType node_ty;
  int constant;
  std::string symbol;
  char op;
  std::unique_ptr<Ast> left;
  std::unique_ptr<Ast> right;

  static std::unique_ptr<Ast> make_constant(int constant) {
    auto ast = std::make_unique<Ast>();
    ast->node_ty = L_AST_CONSTANT;
    ast->constant = constant;
    return ast;
  }
  static std::unique_ptr<Ast> make_symbol(const std::string& symbol) {
    auto ast = std::make_unique<Ast>();
    ast->node_ty = L_AST_SYMBOL;
    ast->symbol = symbol;
    return ast;
  }
  static std::unique_ptr<Ast> make_node(
    char op,
    std::unique_ptr<Ast>&& left,
    std::unique_ptr<Ast>&& right
  ) {
    auto ast = std::make_unique<Ast>();
    ast->node_ty = L_AST_SUBNODE;
    ast->op = op;
    ast->left = std::forward<std::unique_ptr<Ast>>(left);
    ast->right = std::forward<std::unique_ptr<Ast>>(right);
    return ast;
  }

  inline bool is_constant() const {
    return node_ty == L_AST_CONSTANT;
  }
  inline bool is_node(char expected_op = '\0') const {
    return node_ty == L_AST_SUBNODE && (expected_op == '\0' || expected_op == op);
  }
  inline bool is_associative_node() const {
    return is_node('*') || is_node('/') || is_node('%');
  }
  inline bool is_combinational_node() const {
    return is_node('+');
  }
  inline bool is_symbol() const {
    return node_ty == L_AST_SYMBOL;
  }
  inline bool is_constexpr() const {
    return is_constant() ||
      (is_node() && left->is_constant() && right->is_constant());
  }
};



std::unique_ptr<Ast> parse_expr(Tokenizer& tokenizer);

std::unique_ptr<Ast> parse_factor(Tokenizer& tokenizer) {
  if (tokenizer.next_is_integer()) {
    auto token = tokenizer.next();
    return Ast::make_constant(token.integer);
  }
  if (tokenizer.next_is_symbol()) {
    auto token = tokenizer.next();
    return Ast::make_symbol(token.symbol);
  }
  if (tokenizer.next_is_punctuation('(')) {
    tokenizer.next();
    auto ast = parse_expr(tokenizer);
    if (tokenizer.next_is_punctuation(')')) {
      tokenizer.next();
    } else {
      throw std::logic_error("expected `(`, found other token");
    }
    return ast;
  }
  throw std::logic_error("unexpected token or end of input");
}
std::unique_ptr<Ast> parse_term(Tokenizer& tokenizer) {
  std::unique_ptr<Ast> left = parse_factor(tokenizer);

  while (!tokenizer.empty()) {
    auto match = tokenizer.next_is_punctuation('*') ||
      tokenizer.next_is_punctuation('/') ||
      tokenizer.next_is_punctuation('%');
    if (!match) {
      return left;
    }

    auto op_token = tokenizer.next();
    auto right = parse_factor(tokenizer);

    left = Ast::make_node(op_token.punctuation,
      std::move(left), std::move(right));
  }
  return left;
}
std::unique_ptr<Ast> parse_expr(Tokenizer& tokenizer) {
  std::unique_ptr<Ast> left = parse_term(tokenizer);

  while (!tokenizer.empty()) {
    auto match = tokenizer.next_is_punctuation('+');
    if (!match) {
      return left;
    }

    auto op_token = tokenizer.next();
    auto right = parse_term(tokenizer);

    left = Ast::make_node(op_token.punctuation,
      std::move(left), std::move(right));
  }
  return left;
}




void print_impl(std::stringstream& ss, const std::unique_ptr<Ast>& ast) {
  if (ast->node_ty == L_AST_CONSTANT) {
    ss << ast->constant;
  } else if (ast->node_ty == L_AST_SYMBOL) {
    ss << ast->symbol;
  } else {
    bool need_paren = ast->op != '*' && ast->op != '/' && ast->op != '%';

    ss << "(";
    print_impl(ss, ast->left);
    ss << " " << ast->op << " ";
    print_impl(ss, ast->right);
    ss << ")";
  }
}
std::string print(const std::unique_ptr<Ast>& ast) {
  std::stringstream ss;
  print_impl(ss, ast);
  return ss.str();
}








// Move constant coefficients to the left.
void simplify_prioritize_mul_coefficients(std::unique_ptr<Ast>& ast) {
  if (ast->is_node()) {
    simplify_prioritize_mul_coefficients(ast->left);
    simplify_prioritize_mul_coefficients(ast->right);
    if (ast->is_node('*')) {
      if (!ast->left->is_constant() && ast->right->is_constant()) {
        auto temp = std::move(ast->left);
        ast->left = std::move(ast->right);
        ast->right = std::move(temp);
      }
    }
  }
}

// Fold multiplications.
//
// Depends on `simplify_prioritize_mul_coefficients`.
void simplify_associate_mul(std::unique_ptr<Ast>& ast) {
  if (ast->is_node()) {
    // Ensure the sub-expression has been folded.
    simplify_associate_mul(ast->left);
    simplify_associate_mul(ast->right);

    // `simplify_prioritize_mul_coefficients` ensures that all constant
    // multiplicants are on the left.
    if (ast->is_node('*') && ast->left->is_constant()) {
      // Fold constexpr.
      if (ast->right->is_constant()) {
        ast = Ast::make_constant(ast->left->constant * ast->right->constant);
      }
      // Aggregation of coefficients.
      if (ast->right->is_node('*') && ast->right->left->is_constant()) {
        ast->right->left->constant *= ast->left->constant;
        ast = std::move(ast->right);
      }
      // DO NOT support folding with division because it integral division
      // implicitly gives a floored result.
    }
  }
}

// Fold divisions. The divisors are always on the right.
//
// Depends on `simplify_prioritize_mul_coefficients`.
void simplify_associate_div(std::unique_ptr<Ast>& ast) {
  if (ast->is_node()) {
    simplify_associate_div(ast->left);
    simplify_associate_div(ast->right);

    if (ast->is_node('/') && ast->right->is_constant()) {
      // Fold constexpr.
      if (ast->left->is_constant()) {
        ast = Ast::make_constant(ast->left->constant / ast->right->constant);
      }
      // Aggregation of coefficients.
      if (ast->left->is_node('/') && ast->left->right->is_constant()) {
        ast->left->right->constant *= ast->right->constant;
        ast = std::move(ast->right);
      }
      // Fold multiply-divide patterns. Only do this when the multiplicant is a
      // multiple of the divisor. `simplify_prioritize_mul_coefficients` ensures
      // that all constant multiplicants are on the left.
      if (
        ast->left->is_node('*') && ast->left->left->is_constant() &&
        ast->left->left->constant % ast->right->constant == 0
      ) {
        ast->left->left->constant /= ast->right->constant;
        ast = std::move(ast->left);
      }
    }
  }
}

// Fold modulos. The divisors are always on the right.
//
// Depends on `simplify_prioritize_mul_coefficients`.
void simplify_associate_mod_remove_nop(std::unique_ptr<Ast>& ast, int operand) {
  if (ast->is_node()) {
    simplify_associate_mod_remove_nop(ast->left, operand);
    simplify_associate_mod_remove_nop(ast->right, operand);

    // Complicated cases.
    if (ast->is_node('*') && ast->left->is_constant()) {
      if (ast->left->constant % operand == 0) {
        ast = Ast::make_constant(0);
      }
    }
  }
}
void simplify_associate_mod(std::unique_ptr<Ast>& ast) {
  if (ast->is_node()) {
    simplify_associate_mod(ast->left);
    simplify_associate_mod(ast->right);

    if (ast->is_node('%') && ast->right->is_constant()) {
      // Fold constexpr.
      if (ast->left->is_constant()) {
        ast = Ast::make_constant(ast->left->constant / ast->right->constant);
      }
      // Aggregation of coefficients.
      if (ast->left->is_node('%') && ast->left->right->is_constant()) {
        ast->left->right->constant *= ast->right->constant;
        ast = std::move(ast->right);
      }
      // Fold multiply-modulo patterns. Only do this when the multiplicant is a
      // multiple of the divisor; and in that case the expression always gives a
      // zero. `simplify_prioritize_mul_coefficients` ensures that all constant
      // multiplicants are on the left.
      simplify_associate_mod_remove_nop(ast->left, ast->right->constant);
    }
  }
}

// Remove nops that doesn't have any effect on expression evaluation.
void simplify_remove_nop(std::unique_ptr<Ast>& ast) {
  if (ast->is_node()) {
    simplify_remove_nop(ast->left);
    simplify_remove_nop(ast->right);

    // Nops.
    if (ast->is_node('*') && ast->right->is_constant() && ast->right->constant == 1) {
      ast = std::move(ast->right);
      return;
    }
    if (ast->is_node('/') && ast->right->is_constant() && ast->right->constant == 1) {
      ast = std::move(ast->left);
      return;
    }
    if (ast->is_node('+')) {
      if (ast->right->is_constant() && ast->right->constant == 0) {
        ast = std::move(ast->left);
        return;
      }
      if (ast->left->is_constant() && ast->left->constant == 0) {
        ast = std::move(ast->right);
        return;
      }
    }

    // Reduce to zero.
    if (ast->is_node('*') && ast->right->is_constant() && ast->right->constant == 0) {
      ast = Ast::make_constant(0);
      return;
    }
    if (ast->is_node('%') && ast->right->is_constant() && ast->right->constant == 1) {
      ast = Ast::make_constant(0);
      return;
    }
  }
}

void simplify(std::unique_ptr<Ast>& ast) {
  // DON'T CHANGE THE ORDER HERE.
  simplify_prioritize_mul_coefficients(ast);
  simplify_associate_mul(ast);
  simplify_associate_div(ast);
  simplify_associate_mod(ast);
  simplify_remove_nop(ast);
}





int main(int argc, const char** argv) {
  const char* expr_lit = "(a*8+b*4+c*2+d)%4";

  Tokenizer tokenizer(expr_lit);
  auto ast = parse_expr(tokenizer);
  simplify(ast);
  std::cout << "input: " << expr_lit << std::endl;
  std::cout << "ouptut: " << print(ast) << std::endl;
  std::cout << std::endl;
}
