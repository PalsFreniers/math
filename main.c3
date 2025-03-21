import libc;
import std::io;
import std::math;
import std::ascii;
import std::core::dstring;
import std::collections::map;
import std::collections::list;
import std::core::mem::allocator;

fn void usage(String program) {
        io::printfn("Usage: %s <cmd>", program);
        io::printn("Command list:");
        io::printn("\texec \"expr\" => execute the given expression");
        io::printn("\tshell       => opens math shell");
}

enum NodeType {
        NUMBER,
        VECTOR2,
}

union VarValue {
        double number;
        double[<2>] vec2;
}

struct Var {
        NodeType type;
        VarValue as;
}

def VarMap = Map(<String, Var>);

def TokenList = List(<Token>);

enum TokenKind {
        NONE,
        ADD,
        SUB,
        MUL,
        DIV,
        POW,
        FACTORIAL,
        NUMBER,
        IDENTIFIER,
        PARENTESIS_OPEN,
        PARENTESIS_CLOSE,
        EQUAL,
        COMA,
        OPEN_BRACKET,
        CLOSE_BRACKET,
}

union TokenValue {
        double number;
        String ident;
}

struct Token {
        TokenKind kind;
        TokenValue as;
}

fn TokenList lex(String expr) {
        TokenList lst;
        lst.new_init();
        for(int i = 0; i < expr.len; i++) {
                if(expr[i].is_space()) continue;
                if(expr[i].is_digit() || expr[i] == '.') {
                        usz val;
                        usz fract;
                        while(i < expr.len && expr[i].is_digit()) {
                                val *= 10;
                                val += (usz)expr[i] - '0';
                                i++;
                        }
                        int x;
                        if(i < expr.len && expr[i] == '.') {
                                i++;
                                while(i < expr.len && expr[i].is_digit()) {
                                        fract *= 10;
                                        fract += (usz)expr[i] - '0';
                                        i++;
                                        x++;
                                }
                        }
                        i--;
                        Token tok = Token {
                                .kind = TokenKind.NUMBER,
                                .as = TokenValue {
                                        .number = val + (fract / math::pow(10, x))
                                },
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i].is_alpha() || expr[i] == '_') {
                        DString s = dstring::new();
                        while(i < expr.len && (expr[i].is_alnum() || expr[i] == '_')) {
                                s.append(expr[i]);
                                i++;
                        }
                        String ident = s.str_view().copy();
                        if(i < expr.len && !expr[i].is_space()) i--;
                        if(ident == "PI" || ident == "pi" || ident == "Pi") {
                                ident.free();
                                Token tok = Token {
                                        .kind = TokenKind.NUMBER,
                                        .as = TokenValue {
                                                .number = math::PI,
                                        },
                                };
                                lst.push(tok);
                                continue;
                        } else if(ident == "INF" || ident == "inf" || ident == "Inf") {
                                ident.free();
                                Token tok = Token {
                                        .kind = TokenKind.NUMBER,
                                        .as = TokenValue {
                                                .number = -1.0 / 0.0,
                                        },
                                };
                                lst.push(tok);
                                continue;
                        } else if(ident == "NAN" || ident == "nan" || ident == "NaN") {
                                ident.free();
                                Token tok = Token {
                                        .kind = TokenKind.NUMBER,
                                        .as = TokenValue {
                                                .number = 0.0 / 0.0,
                                        },
                                };
                                lst.push(tok);
                                continue;
                        }
                        Token tok = Token {
                                .kind = TokenKind.IDENTIFIER,
                                .as = TokenValue {
                                        .ident = ident,
                                },
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '+') {
                        Token tok = Token {
                                .kind = TokenKind.ADD,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '-') {
                        Token tok = Token {
                                .kind = TokenKind.SUB,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '*') {
                        Token tok = Token {
                                .kind = TokenKind.MUL,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '/') {
                        Token tok = Token {
                                .kind = TokenKind.DIV,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '(') {
                        Token tok = Token {
                                .kind = TokenKind.PARENTESIS_OPEN,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == ')') {
                        Token tok = Token {
                                .kind = TokenKind.PARENTESIS_CLOSE,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '{') {
                        Token tok = Token {
                                .kind = TokenKind.OPEN_BRACKET,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '}') {
                        Token tok = Token {
                                .kind = TokenKind.CLOSE_BRACKET,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '^') {
                        Token tok = Token {
                                .kind = TokenKind.POW,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '!') {
                        Token tok = Token {
                                .kind = TokenKind.FACTORIAL,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == '=') {
                        Token tok = Token {
                                .kind = TokenKind.EQUAL,
                        };
                        lst.push(tok);
                        continue;
                }
                if(expr[i] == ',') {
                        Token tok = Token {
                                .kind = TokenKind.COMA,
                        };
                        lst.push(tok);
                        continue;
                }
                unreachable("Unable to lex expression");
        }
        return lst;
}

def NodeList = List(<Node*>);

struct NodeFunction {
        String name;
        NodeList params;
}

struct NodeBinop {
        Node* lhs;
        Node* rhs;
}

union NodeAs {
        double number;
        Node*[<2>] vec2;
        Node* prio;
        String ident;
        NodeBinop binop;
        NodeFunction func;
}

enum NodeKind {
        NONE,
        NUMBER,
        VECTOR2,
        ADD,
        SUB,
        MUL,
        DIV,
        POW,
        FACTORIAL,
        PRIO,
        IDENTIFIER,
        ASSIGN,
        FUNCTION,
}

struct Node {
        NodeKind kind;
        NodeAs as;
        NodeType type;
}

fn Node* TokenList.parse(&self, Allocator alloc) {
        return self.parseAssignExpr(alloc);
}

fn Node* TokenList.parseAssignExpr(&self, Allocator alloc) {
        Node* left = self.parseLv1Expr(alloc);
        Token! y = self.first();
        if(catch y) return left;
        Token curr = y;
        if(left.kind == NodeKind.IDENTIFIER && curr.kind == TokenKind.EQUAL) {
                Token op = self.pop_first()!!;
                Node* right = self.parseLv1Expr(alloc);
                left = exportNodeBinop(alloc, op, left, right);
                Token! x = self.first();
                if(catch x) return left;
                curr = x;
        }
        return left;
}

fn Node* TokenList.parseLv1Expr(&self, Allocator alloc) {
        Node* left = self.parseLv2Expr(alloc);
        Token! y = self.first();
        if(catch y) return left;
        Token curr = y;
        while(curr.kind == TokenKind.ADD || curr.kind == TokenKind.SUB) {
                Token op = self.pop_first()!!;
                Node* right = self.parseLv2Expr(alloc);
                left = exportNodeBinop(alloc, op, left, right);
                Token! x = self.first();
                if(catch x) return left;
                curr = x;
        }
        return left;
}

fn Node* TokenList.parseLv2Expr(&self, Allocator alloc) {
        Node* left = self.parseLv3Expr(alloc);
        Token! y = self.first();
        if(catch y) return left;
        Token curr = y;
        while(curr.kind == TokenKind.DIV || curr.kind == TokenKind.MUL) {
                Token op = self.pop_first()!!;
                Node* right = self.parseLv3Expr(alloc);
                left = exportNodeBinop(alloc, op, left, right);
                Token! x = self.first();
                if(catch x) return left;
                curr = x;
        }
        return left;
}

fn Node* TokenList.parseLv3Expr(&self, Allocator alloc) {
        Node* left = self.parseLv4Expr(alloc);
        Token! y = self.first();
        if(catch y) return left;
        Token curr = y;
        while(curr.kind == TokenKind.POW) {
                Token op = self.pop_first()!!;
                Node* right = self.parseLv4Expr(alloc);
                left = exportNodeBinop(alloc, op, left, right);
                Token! x = self.first();
                if(catch x) return left;
                curr = x;
        }
        return left;
}

fn Node* TokenList.parseLv4Expr(&self, Allocator alloc) {
        Node* left = self.parseLv5Expr(alloc);
        Token! y = self.first();
        if(catch y) return left;
        Token curr = y;
        if(curr.kind == TokenKind.FACTORIAL) {
                return nodeFactorial(alloc, left);
        }
        return left;
}

fn Node* TokenList.parseLv5Expr(&self, Allocator alloc) {
        Token curr = self.first()!!;
        if(curr.kind == TokenKind.SUB) {
                Token op = self.pop_first()!!;
                Node* right = self.parseLv5Expr(alloc);
                return exportNodeBinop(alloc, op, nodeNumber(alloc, 0), right);
        } else if(curr.kind == TokenKind.PARENTESIS_OPEN) {
                self.pop_first()!!;
                Node* right = self.parseLv1Expr(alloc);
                curr = self.first()!!;
                assert(curr.kind == TokenKind.PARENTESIS_CLOSE, "unclosed parenthesis");
                self.pop_first()!!;
                return nodePrio(alloc, right);
        } else if(curr.kind == TokenKind.OPEN_BRACKET) {
                self.pop_first()!!;
                Node* first = self.parseLv1Expr(alloc);
                curr = self.first()!!;
                assert(curr.kind == TokenKind.COMA, "missing coma");
                self.pop_first()!!;
                Node* second = self.parseLv1Expr(alloc);
                curr = self.first()!!;
                assert(curr.kind == TokenKind.CLOSE_BRACKET, "unclosed vector2");
                self.pop_first()!!;
                return nodeVec2(alloc, first, second);
        } else if(curr.kind == TokenKind.IDENTIFIER) {
                Token name = self.pop_first()!!;
                Token! paren = self.first();
                if(catch paren) {
                        self.push_front(name);
                        return self.parseLvMaxExpr(alloc);
                }
                if(paren.kind != TokenKind.PARENTESIS_OPEN) {
                        self.push_front(name);
                        return self.parseLvMaxExpr(alloc);
                }
                self.pop_first()!!;
                NodeList l;
                l.new_init(2, alloc);
                curr = self.first()!!; 
                while(curr.kind != TokenKind.PARENTESIS_CLOSE) {
                        Node* tmp = self.parseLv1Expr(alloc);
                        l.push(tmp);
                        Token coma = self.first()!!;
                        assert(coma.kind == TokenKind.COMA || coma.kind == TokenKind.PARENTESIS_CLOSE, "function parameters must be separated by comas");
                        if(coma.kind == TokenKind.COMA) self.pop_first()!!;
                        curr = self.first()!!;
                }
                self.pop_first()!!;
                NodeFunction n;
                n.name = name.as.ident;
                n.params = l;
                return nodeFunc(alloc, n);
        } else {
                return self.parseLvMaxExpr(alloc);
        }
}

fn Node* TokenList.parseLvMaxExpr(&self, Allocator alloc) {
        Token curr = self.pop_first()!!;
        if(curr.kind == TokenKind.NUMBER) return nodeNumber(alloc, curr.as.number);
        if(curr.kind == TokenKind.IDENTIFIER) return nodeIdentifier(alloc, curr.as.ident);
        unreachable("unable to parse number");
}

fn Node* exportNodeBinop(Allocator alloc, Token op, Node* lhs, Node* rhs) {
        switch(op.kind) {
                case TokenKind.ADD:
                        return nodeAdd(alloc, lhs, rhs);
                case TokenKind.SUB:
                        return nodeSub(alloc, lhs, rhs);
                case TokenKind.MUL:
                        return nodeMul(alloc, lhs, rhs);
                case TokenKind.DIV:
                        return nodeDiv(alloc, lhs, rhs);
                case TokenKind.POW:
                        return nodePow(alloc, lhs, rhs);
                case TokenKind.EQUAL:
                        return nodeAssign(alloc, lhs, rhs);
                default:
                        unreachable("exportNodeBinop");
        }
}

fn Node* nodeIdentifier(Allocator alloc, String number) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.ident = number;
        ret.kind = NodeKind.IDENTIFIER;
        return ret;
}

fn Node* nodeNumber(Allocator alloc, double number) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.number = number;
        ret.kind = NodeKind.NUMBER;
        ret.type = NodeType.NUMBER;
        return ret;
}

fn Node* nodePrio(Allocator alloc, Node* expr) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.prio = expr;
        ret.kind = NodeKind.PRIO;
        return ret;
}

fn Node* nodeVec2(Allocator alloc, Node* x, Node* y) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.vec2 = {x, y};
        ret.kind = NodeKind.VECTOR2;
        ret.type = NodeType.VECTOR2;
        return ret;
}

fn Node* nodeFactorial(Allocator alloc, Node* expr) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.prio = expr;
        ret.kind = NodeKind.FACTORIAL;
        return ret;
}

fn Node* nodeFunc(Allocator alloc, NodeFunction params) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.func = params;
        ret.kind = NodeKind.FUNCTION;
        return ret;
}

fn Node* nodeBinop(Allocator alloc, NodeKind kind, Node* lhs, Node* rhs) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.binop.lhs = lhs;
        ret.as.binop.rhs = rhs;
        ret.kind = kind;
        return ret;
}

macro Node* nodeAdd(Allocator alloc, Node* lhs, Node* rhs) => nodeBinop(alloc, NodeKind.ADD, lhs, rhs);
macro Node* nodeSub(Allocator alloc, Node* lhs, Node* rhs) => nodeBinop(alloc, NodeKind.SUB, lhs, rhs);
macro Node* nodeMul(Allocator alloc, Node* lhs, Node* rhs) => nodeBinop(alloc, NodeKind.MUL, lhs, rhs);
macro Node* nodeDiv(Allocator alloc, Node* lhs, Node* rhs) => nodeBinop(alloc, NodeKind.DIV, lhs, rhs);
macro Node* nodePow(Allocator alloc, Node* lhs, Node* rhs) => nodeBinop(alloc, NodeKind.POW, lhs, rhs);
macro Node* nodeAssign(Allocator alloc, Node* lhs, Node* rhs) => nodeBinop(alloc, NodeKind.ASSIGN, lhs, rhs);

fn Node* NodeFunction.eval(&func, VarMap *vars, Allocator alloc) {
        switch(func.name) {
                case "sqrt":
                        if(func.params.len() != 1) {
                                io::printn("[RUNTIME ERROR] => square root function should have only one parameter");
                                return null;
                        }
                        Node* val = func.params[0].eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        double num = val.as.number;
                        if(num < 0) {
                                io::printn("[RUNTIME ERROR] => square root of a negative nuber is not defined");
                                return null;
                        }
                        if(num == 0) {
                                io::printn("[RUNTIME ERROR] => square root of zero is not defined");
                                return null;
                        }
                        return nodeNumber(alloc, math::sqrt(num));
                case "sin":
                        if(func.params.len() != 1) {
                                io::printn("[RUNTIME ERROR] => sin function should have only one parameter");
                                return null;
                        }
                        Node* val = func.params[0].eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        double num = val.as.number;
                        return nodeNumber(alloc, math::sin(num));
                case "cos":
                        if(func.params.len() != 1) {
                                io::printn("[RUNTIME ERROR] => cos function should have only one parameter");
                                return null;
                        }
                        Node* val = func.params[0].eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        double num = val.as.number;
                        return nodeNumber(alloc, math::cos(num));
                case "tan":
                        if(func.params.len() != 1) {
                                io::printn("[RUNTIME ERROR] => tan function should have only one parameter");
                                return null;
                        }
                        Node* val = func.params[0].eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        double num = val.as.number;
                        return nodeNumber(alloc, math::tan(num));
                case "asin":
                        if(func.params.len() != 1) {
                                io::printn("[RUNTIME ERROR] => asin function should have only one parameter");
                                return null;
                        }
                        Node* val = func.params[0].eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        double num = val.as.number;
                        return nodeNumber(alloc, math::asin(num));
                case "acos":
                        if(func.params.len() != 1) {
                                io::printn("[RUNTIME ERROR] => acos function should have only one parameter");
                                return null;
                        }
                        Node* val = func.params[0].eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        double num = val.as.number;
                        return nodeNumber(alloc, math::acos(num));
                case "atan":
                        if(func.params.len() != 1) {
                                io::printn("[RUNTIME ERROR] => atan function should have only one parameter");
                                return null;
                        }
                        Node* val = func.params[0].eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        double num = val.as.number;
                        return nodeNumber(alloc, math::atan(num));
                case "dot":
                        if(func.params.len() != 2) {
                                io::printn("[RUNTIME ERROR] => dot function should have only two parameter");
                                return null;
                        }
                        Node* v1 = func.params[0].eval(vars, alloc);
                        if(v1 == null || v1.kind != NodeKind.VECTOR2) return null;
                        double[<2>] v1n = {v1.as.vec2.x.as.number, v1.as.vec2.y.as.number};
                        Node* v2 = func.params[1].eval(vars, alloc);
                        if(v2 == null || v2.kind != NodeKind.VECTOR2) return null;
                        double[<2>] v2n = {v2.as.vec2.x.as.number, v2.as.vec2.y.as.number};
                        return nodeNumber(alloc, v1n.dot(v2n));
                case "printExprs":
                        if(func.params.len() < 1) {
                                io::printn("[RUNTIME WARNING] => no expression to print");
                                return null;
                        }
                        foreach(p: func.params) {
                                p.print();
                                io::printfn("");
                                VarMap fakeVars;
                                Node* tmp = p.eval(&fakeVars, alloc);
                                fakeVars.free();
                                io::print("\t");
                                if(tmp == null) {
                                        io::printfn("resulted in null");
                                } else if(math::is_nan(tmp.as.number)) {
                                        io::printn("result: not a number");
                                } else if(math::is_inf(tmp.as.number)) {
                                        if(tmp.as.number < 0) {
                                                io::printn("result: +Inf");
                                        } else {
                                                io::printn("result: -Inf");
                                        }
                                } else {
                                        io::printfn("result: %f", tmp.as.number);
                                }
                        }
                        return null;
                default:
                        io::printfn("[RUNTIME WARNING] => unable to find function %s", func.name);
                        return null;
        }
}

macro bool checkNodeIsLeaf(Node* rhs) => rhs.kind != NodeKind.NUMBER && rhs.kind != NodeKind.VECTOR2;
macro Node* nodeVec2N(alloc, x, y) => nodeVec2(alloc, nodeNumber(alloc, x), nodeNumber(alloc, y));

fn Node* Node.eval(&tree, VarMap *vars, Allocator alloc) {
        switch(tree.kind) {
                case NodeKind.NUMBER:
                        return tree;
                case NodeKind.VECTOR2:
                        Node *x = tree.as.vec2.x.eval(vars, alloc);
                        if(x.kind != NodeKind.NUMBER) return null;
                        Node *y = tree.as.vec2.y.eval(vars, alloc);
                        if(x.type != y.type) return null;
                        return nodeVec2(alloc, x, y);
                case NodeKind.IDENTIFIER:
                        String name = tree.as.ident;
                        Var! val = (*vars)[name];
                        if(catch val) return nodeNumber(alloc, 0);
                        if(val.type == NodeType.NUMBER) {
                                return nodeNumber(alloc, val.as.number);
                        } else if(val.type == NodeType.VECTOR2) {
                                return nodeVec2N(alloc, val.as.vec2.x, val.as.vec2.y);
                        }
                        return null;
                case NodeKind.ASSIGN:
                        Node* name = tree.as.binop.lhs;
                        if(name.kind != NodeKind.IDENTIFIER) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || checkNodeIsLeaf(rhs)) return null;
                        Var v;
                        v.type = rhs.type;
                        if(v.type == NodeType.NUMBER) v.as.number = rhs.as.number;
                        if(v.type == NodeType.VECTOR2) v.as.vec2 = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                        (*vars)[name.as.ident] = v;
                        return rhs;
                case NodeKind.ADD:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || checkNodeIsLeaf(lhs)) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || checkNodeIsLeaf(rhs)) return null;
                        Node* ret;
                        if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.NUMBER) {
                                ret = nodeNumber(alloc, lhs.as.number + rhs.as.number);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.NUMBER) {
                                double[<2>] tmp = {lhs.as.vec2.x.as.number, lhs.as.vec2.y.as.number};
                                tmp += rhs.as.number;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp += lhs.as.number;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                double[<2>] tmp2 = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp += tmp2;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        }
                        return ret;
                case NodeKind.SUB:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || checkNodeIsLeaf(lhs)) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || checkNodeIsLeaf(rhs)) return null;
                        Node* ret;
                        if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.NUMBER) {
                                ret = nodeNumber(alloc, lhs.as.number - rhs.as.number);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.NUMBER) {
                                double[<2>] tmp = {lhs.as.vec2.x.as.number, lhs.as.vec2.y.as.number};
                                tmp -= rhs.as.number;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp.x = lhs.as.number - tmp.x;
                                tmp.y = lhs.as.number - tmp.y;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                double[<2>] tmp2 = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp -= tmp2;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        }
                        return ret;
                case NodeKind.MUL:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || checkNodeIsLeaf(lhs)) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || checkNodeIsLeaf(rhs)) return null;
                        Node* ret;
                        if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.NUMBER) {
                                ret = nodeNumber(alloc, lhs.as.number * rhs.as.number);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.NUMBER) {
                                double[<2>] tmp = {lhs.as.vec2.x.as.number, lhs.as.vec2.y.as.number};
                                tmp *= rhs.as.number;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp *= lhs.as.number;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                double[<2>] tmp2 = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp *= tmp2;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        }
                        return ret;
                case NodeKind.DIV:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || checkNodeIsLeaf(lhs)) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || checkNodeIsLeaf(rhs)) return null;
                        Node* ret;
                        if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.NUMBER) {
                                ret = nodeNumber(alloc, lhs.as.number / rhs.as.number);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.NUMBER) {
                                double[<2>] tmp = {lhs.as.vec2.x.as.number, lhs.as.vec2.y.as.number};
                                tmp /= rhs.as.number;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.NUMBER && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp.x = lhs.as.number / tmp.x;
                                tmp.y = lhs.as.number / tmp.y;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        } else if(lhs.type == NodeType.VECTOR2 && rhs.type == NodeType.VECTOR2) {
                                double[<2>] tmp = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                double[<2>] tmp2 = {rhs.as.vec2.x.as.number, rhs.as.vec2.y.as.number};
                                tmp /= tmp2;
                                ret = nodeVec2N(alloc, tmp.x, tmp.y);
                        }
                        return ret;
                case NodeKind.POW:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || checkNodeIsLeaf(lhs)) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || rhs.kind == NodeKind.NUMBER) return null;
                        if(lhs.kind == NodeKind.NUMBER) return nodeNumber(alloc, math::pow(lhs.as.number, rhs.as.number));
                        if(lhs.kind == NodeKind.VECTOR2) return nodeVec2N(alloc, math::pow(lhs.as.vec2.x.as.number, rhs.as.number), math::pow(lhs.as.vec2.x.as.number, rhs.as.number));
                        return null;
                case NodeKind.FACTORIAL:
                        Node* val = tree.as.prio.eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        if(val.as.number <= 0 || math::is_nan(val.as.number)) return null;
                        return nodeNumber(alloc, math::tgamma(val.as.number + 1));
                case NodeKind.PRIO:
                        return tree.as.prio.eval(vars, alloc);
                case NodeKind.FUNCTION:
                        return tree.as.func.eval(vars, alloc);
                default:
                        unreachable("eval");
        }
}

fn void Token.print(&t) {
        switch(t.kind) {
                case TokenKind.NUMBER:
                        io::printfn("NUMBER(%f)", t.as.number);
                case TokenKind.IDENTIFIER:
                        io::printfn("IDENTIFIER(%s)", t.as.ident);
                case TokenKind.ADD:
                        io::printn("ADD");
                case TokenKind.SUB:
                        io::printn("SUB");
                case TokenKind.DIV:
                        io::printn("DIV");
                case TokenKind.MUL:
                        io::printn("MUL");
                case TokenKind.POW:
                        io::printn("POW");
                case TokenKind.PARENTESIS_OPEN:
                        io::printn("PARENTESIS_OPEN");
                case TokenKind.PARENTESIS_CLOSE:
                        io::printn("PARENTESIS_CLOSE");
                case TokenKind.FACTORIAL:
                        io::printn("FACTORIAL");
                case TokenKind.EQUAL:
                        io::printn("EQUAL");
                case TokenKind.COMA:
                        io::printn("COMA");
                case TokenKind.OPEN_BRACKET:
                        io::printn("OPEN_BRACKET");
                case TokenKind.CLOSE_BRACKET:
                        io::printn("CLOSE_BRACKET");
                default:
                        unreachable("Token.print");
        }
}

fn void Node.print(&self) {
        switch(self.kind) {
                case NodeKind.NUMBER:
                        io::printf("%f", self.as.number);
                case NodeKind.IDENTIFIER:
                        io::printf("%s", self.as.ident);
                case NodeKind.ADD:
                        io::print("add(");
                        self.as.binop.lhs.print();
                        io::print(", ");
                        self.as.binop.rhs.print();
                        io::print(")");
                case NodeKind.SUB:
                        io::print("sub(");
                        self.as.binop.lhs.print();
                        io::print(", ");
                        self.as.binop.rhs.print();
                        io::print(")");
                case NodeKind.DIV:
                        io::print("div(");
                        self.as.binop.lhs.print();
                        io::print(", ");
                        self.as.binop.rhs.print();
                        io::print(")");
                case NodeKind.MUL:
                        io::print("mul(");
                        self.as.binop.lhs.print();
                        io::print(", ");
                        self.as.binop.rhs.print();
                        io::print(")");
                case NodeKind.POW:
                        io::print("pow(");
                        self.as.binop.lhs.print();
                        io::print(", ");
                        self.as.binop.rhs.print();
                        io::print(")");
                case NodeKind.VECTOR2:
                        io::print("{");
                        self.as.vec2.x.print();
                        io::print(", ");
                        self.as.vec2.y.print();
                        io::print("}");
                case NodeKind.FACTORIAL:
                        io::print("factorial(");
                        self.as.binop.lhs.print();
                        io::print(")");
                case NodeKind.ASSIGN:
                        io::print("assign(");
                        self.as.binop.lhs.print();
                        io::print(")");
                case NodeKind.PRIO:
                        self.as.prio.print();
                case NodeKind.FUNCTION:
                        io::printf("%s(", self.as.func.name);
                        foreach(i, p : self.as.func.params) {
                                p.print();
                                if(i < self.as.func.params.len() - 1) io::print(", ");
                        }
                        io::print(")");
                default:
                        unreachable("Node.print");
        }
}

fn int execOne(String s, VarMap *vars) {
        SimpleHeapAllocator heap;
        heap.init(&__malloc__);
        DynamicArenaAllocator alloc;
        alloc.init(4096, &heap);
        defer alloc.free();
        TokenList toks = lex(s);
        //foreach(t : toks) {
        //        t.print();
        //}
        if(toks.size == 0) return 0;
        Node* expr = toks.parse(&alloc);
        //expr.print();
        io::printn("");
        Node* res = expr.eval(vars, &alloc);
        if(res == null || checkNodeIsLeaf(res)) {
                io::printn("result: null");
                return 1;
        }
        io::print("result: ");
        res.print();
        io::printn("");
        return 0;
}

fn int main(String[] args) {
        String program = args[0];
        if(args.len <= 1) {
                usage(program);
                return 1;
        }
        VarMap vars;
        if(args[1] == "exec") {
                if(args.len <= 2) {
                        usage(program);
                        return 1;
                }
                int x = execOne(args[2], &vars);
                vars.free();
                return x;
        }
        if(args[1] == "shell") {
                String str = "";
                while(true) {
                        io::print("=>> ");
                        str = io::readline()!!;
                        if(str == "quit" || str == "exit") break;
                        if(str == "clear") {
                                io::printn("\ec");
                                mem::free(str);
                                continue;
                        }
                        execOne(str, &vars);
                        mem::free(str);
                }
        }
        vars.free();
        return 0;
}

fn char[]! __malloc__(usz size) => mem::alloc_array(char, size);

module std::math;

extern fn double tgamma(double val);
