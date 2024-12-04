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

def VarMap = Map(<String, double>);

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
}

union TokenValue {
        double number;
        String ident;
}

struct Token {
        TokenKind kind;
        TokenValue as;
}

struct Parser {
        bool in_digit;
        bool in_fract;
}

macro void Parser.reset(&self) {
        self.in_fract = false;
        self.in_digit = false;
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
                        if(ident == "PI" || ident == "pi") {
                                ident.free();
                                Token tok = Token {
                                        .kind = TokenKind.NUMBER,
                                        .as = TokenValue {
                                                .number = math::PI,
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
                unreachable("Unable to lex expression");
        }
        return lst;
}

struct NodeBinop {
        Node* lhs;
        Node* rhs;
}

union NodeAs {
        double number;
        Node* prio;
        String ident;
        NodeBinop binop;
}

enum NodeKind {
        NONE,
        NUMBER,
        ADD,
        SUB,
        MUL,
        DIV,
        POW,
        FACTORIAL,
        PRIO,
        IDENTIFIER,
        ASSIGN,
}

struct Node {
        NodeKind kind;
        NodeAs as;
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
                Node* right = self.parseLvMaxExpr(alloc);
                return exportNodeBinop(alloc, op, nodeNumber(alloc, 0), right);
        } else if(curr.kind == TokenKind.PARENTESIS_OPEN) {
                Token op = self.pop_first()!!;
                Node* right = self.parseLv1Expr(alloc);
                curr = self.first()!!;
                assert(curr.kind == TokenKind.PARENTESIS_CLOSE, "unclosed parenthesis");
                self.pop_first()!!;
                return nodePrio(alloc, right);
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
        return ret;
}

fn Node* nodePrio(Allocator alloc, Node* expr) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.prio = expr;
        ret.kind = NodeKind.PRIO;
        return ret;
}

fn Node* nodeFactorial(Allocator alloc, Node* expr) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.prio = expr;
        ret.kind = NodeKind.FACTORIAL;
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

fn Node *Node.eval(&tree, VarMap *vars, Allocator alloc) {
        switch(tree.kind) {
                case NodeKind.NUMBER:
                        return tree;
                case NodeKind.IDENTIFIER:
                        String name = tree.as.ident;
                        double! val = (*vars)[name];
                        if(catch val) return nodeNumber(alloc, 0);
                        return nodeNumber(alloc, val);
                case NodeKind.ASSIGN:
                        Node* name = tree.as.binop.lhs;
                        if(name.kind != NodeKind.IDENTIFIER) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || rhs.kind != NodeKind.NUMBER) return null;
                        (*vars)[name.as.ident] = rhs.as.number;
                        return nodeNumber(alloc, rhs.as.number);
                case NodeKind.ADD:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || lhs.kind != NodeKind.NUMBER) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || rhs.kind != NodeKind.NUMBER) return null;
                        return nodeNumber(alloc, lhs.as.number + rhs.as.number);
                case NodeKind.SUB:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || lhs.kind != NodeKind.NUMBER) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || rhs.kind != NodeKind.NUMBER) return null;
                        return nodeNumber(alloc, lhs.as.number - rhs.as.number);
                case NodeKind.MUL:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || lhs.kind != NodeKind.NUMBER) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || rhs.kind != NodeKind.NUMBER) return null;
                        return nodeNumber(alloc, lhs.as.number * rhs.as.number);
                case NodeKind.DIV:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || lhs.kind != NodeKind.NUMBER) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || rhs.kind != NodeKind.NUMBER) return null;
                        return nodeNumber(alloc, lhs.as.number / rhs.as.number);
                case NodeKind.POW:
                        Node* lhs = tree.as.binop.lhs.eval(vars, alloc);
                        if(lhs == null || lhs.kind != NodeKind.NUMBER) return null;
                        Node* rhs = tree.as.binop.rhs.eval(vars, alloc);
                        if(rhs == null || rhs.kind != NodeKind.NUMBER) return null;
                        return nodeNumber(alloc, math::pow(lhs.as.number, rhs.as.number));
                case NodeKind.FACTORIAL:
                        Node* val = tree.as.prio.eval(vars, alloc);
                        if(val == null || val.kind != NodeKind.NUMBER) return null;
                        if(val.as.number <= 0 || math::is_nan(val.as.number)) return null;
                        return nodeNumber(alloc, math::tgamma(val.as.number + 1));
                case NodeKind.PRIO:
                        return tree.as.prio.eval(vars, alloc);
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
        Node* expr = toks.parse(&alloc);
        //expr.print();
        io::printn("");
        Node* res = expr.eval(vars, &alloc);
        if(res == null || res.kind != NodeKind.NUMBER) {
                io::eprintn("error: not a number");
                return 1;
        }
        io::printfn("result: %f", res.as.number);
        return 0;
}

fn int main(String[] args) {
        String program = args[0];
        if(args.len <= 1) {
                usage(program);
                return 1;
        }
        VarMap vars = map::new(<String, double>)();
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
                        if(str == "quit") break;
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
