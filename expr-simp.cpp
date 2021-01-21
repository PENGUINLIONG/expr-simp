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
    bool is_alphabet = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
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
  // TODO: (penguinliong) Better error handling?
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

  // L_AST_CONSTANT
  int constant;
  int range_hint_high;

  // L_AST_SYMBOL
  std::string symbol;

  // L_AST_SUBNODE
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
    ast->range_hint_high = 0;
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




void hint_symbol(std::unique_ptr<Ast>& ast, const std::string& symbol, int high) {
  if (ast->is_symbol() && ast->symbol == symbol) {
    ast->range_hint_high = high;
  }
  if (ast->is_node()) {
    hint_symbol(ast->left, symbol, high);
    hint_symbol(ast->right, symbol, high);
  }
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

// Depends on `simplify_prioritize_mul_coefficients`.
bool simplify_is_multiple_of(std::unique_ptr<Ast>& ast, int divisor) {
  if (ast->is_symbol()) {
    return false;
  }
  if (ast->is_constant()) {
    return ast->constant % divisor == 0;
  }
  if (ast->is_node('*')) {
    return simplify_is_multiple_of(ast->left, divisor);
  }
  if (ast->is_combinational_node()) {
    return simplify_is_multiple_of(ast->left, divisor) &&
      simplify_is_multiple_of(ast->right, divisor);
  }
  return false;
}

// Get all coefficients in a polynomial. The function returns no element if the
// sub-expression contains any division or modulo.
bool simplify_collect_coefficients_impl(std::unique_ptr<Ast>& ast, std::vector<int>& out) {
  if (ast->is_symbol()) {
    out.push_back(1);
    return true;
  }
  if (ast->is_constant()) {
    out.push_back(ast->constant);
    return true;
  }
  if (ast->is_node('*')) {
    return simplify_collect_coefficients_impl(ast->left, out);
  }
  if (ast->is_combinational_node()) {
    return simplify_collect_coefficients_impl(ast->left, out) &&
      simplify_collect_coefficients_impl(ast->right, out);
  }
  return false;
}
std::vector<int> simplify_collect_coefficients(std::unique_ptr<Ast>& ast) {
  std::vector<int> out;
  return simplify_collect_coefficients_impl(ast, out) ? out : std::vector<int> {};
}
int simplify_get_coefficient_gcd(std::unique_ptr<Ast>& ast) {
  auto gcd = [](int a, int b) {
    while (a != b) {
      if (a > b) {
        a -= b;
      } else {
        b -= a;
      }
    }
    return a;
  };
  auto coes = simplify_collect_coefficients(ast);
  if (coes.empty()) { return 1; }

  auto out = coes[0];
  for (auto coe : coes) {
    out = gcd(coe, out);
  }
  return out;
}

// Get the upper bound of the values in this sub-expression. Retuens 0 if one
// term has never been hinted.
int simplify_upper_bound_of(const std::unique_ptr<Ast>& ast) {
  if (ast->is_constant()) {
    return ast->constant;
  }
  if (ast->is_symbol()) {
    // Can be zero, and we let it contaminate the other numbers to give a zero
    // result.
    return ast->range_hint_high;
  }
  if (ast->is_node('*')) {
    return simplify_upper_bound_of(ast->left) *
      simplify_upper_bound_of(ast->right);
  }
  if (ast->is_node('%')) {
    return simplify_upper_bound_of(ast->right);
  }
  if (ast->is_node('/')) {
    auto divisor = simplify_upper_bound_of(ast->right);
    return (simplify_upper_bound_of(ast->left) + divisor - 1) / divisor;
  }
  if (ast->is_node('+')) {
    return simplify_upper_bound_of(ast->left) + simplify_upper_bound_of(ast->right);
  }
  throw std::logic_error("not implemented yet");
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
void simplify_associate_div_remove_nop(std::unique_ptr<Ast>& ast, int operand) {
  if (ast->is_node()) {
    simplify_associate_div_remove_nop(ast->left, operand);
    simplify_associate_div_remove_nop(ast->right, operand);

    // Complicated cases.
    if (ast->is_node('*') && ast->left->is_constant()) {
      if (ast->left->constant % operand == 0) {
        ast->left->constant /= operand;
      }
    }
  }
}
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
      if (simplify_is_multiple_of(ast->left, ast->right->constant)) {
        simplify_associate_div_remove_nop(ast->left, ast->right->constant);
        ast = std::move(ast->left);
      }
      // THE FOLLOWING SECTION MUST PRECEDE THE ONE ABOVE; OTHERWISE THE STACK
      // WOULD OVERFLOW.
      // In case the left-expression share a common divisor, we can extract the
      // common divisor to the right-constant. In some cases, the common divisor
      // can be a coefficient in some terms in the left-expression. So some
      // multiplication can be saved.
      auto gcd = simplify_get_coefficient_gcd(ast->left);
      if (gcd != 1 && simplify_is_multiple_of(ast->right, gcd)) {
        ast->right->constant /= gcd;
        auto left = Ast::make_node('/', std::move(ast->left), Ast::make_constant(gcd));
        simplify_associate_div(left);
        ast->left = std::move(left);
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
      // If the upper bound is hinted never reaching the modulo divisor, the
      // modulo can be removed.
      auto upper_bound = simplify_upper_bound_of(ast->left);
      if (upper_bound > 0 && upper_bound <= ast->right->constant) {
        ast = std::move(ast->left);
      }
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
  const char* expr_lit = "(((((((group_id2)*1048576)+((local_id2)*16384))+((group_id1)*1024))+((local_id1)*512))+((group_id0)*32))+((local_id0)*8))/4%64";

  Tokenizer tokenizer(expr_lit);
  auto ast = parse_expr(tokenizer);
  hint_symbol(ast, "global_id0", 2);
  hint_symbol(ast, "local_id0", 2);
  simplify(ast);
  std::cout << "input: " << expr_lit << std::endl;
  std::cout << "ouptut: " << print(ast) << std::endl;
  std::cout << std::endl;
}
