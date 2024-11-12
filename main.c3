import libc;
import std::io;
import std::core::mem::allocator;

fn void usage(String program) {
        io::printfn("Usage: %s <cmd>", program);
        io::printn("Command list:");
        io::printn("\texec \"expr\" => execute the given expression");
        io::printn("\tshell         => opens math shell");
}

struct NodeBinop {
        Node* lhs;
        Node* rhs;
}

union NodeAs {
        float number;
        NodeBinop binop;
}

enum NodeKind {
        NUMBER,
        ADD,
}

struct Node {
        NodeKind kind;
        NodeAs as;
}

fn Node* nodeNumber(Allocator alloc, float number) {
        Node* ret = alloc.acquire(Node.sizeof, AllocInitType.ZERO)!!;
        ret.as.number = number;
        ret.kind = NodeKind.NUMBER;
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

fn Node *eval(Allocator alloc, Node* tree) {
        switch(tree.kind) {
                case NodeKind.NUMBER:
                        return tree;
                case NodeKind.ADD:
                        Node* lhs = eval(alloc, tree.as.binop.lhs);
                        if(lhs == null || lhs.kind != NodeKind.NUMBER) return null;
                        Node* rhs = eval(alloc, tree.as.binop.rhs);
                        if(rhs == null || rhs.kind != NodeKind.NUMBER) return null;
                        return nodeNumber(alloc, lhs.as.number + rhs.as.number);
                default:
                        unreachable("eval");
        }
}

fn int main(String[] args) {
        String program = args[0];
        if(args.len <= 1) {
                usage(program);
                return 1;
        }
        SimpleHeapAllocator heap;
        heap.init(&__malloc__);
        DynamicArenaAllocator alloc;
        alloc.init(4096, &heap);
        defer alloc.free();
        Node* expr = nodeAdd(&alloc, nodeNumber(&alloc, 12), nodeNumber(&alloc, 5));
        Node* res = eval(&alloc, expr);
        if(res == null || res.kind != NodeKind.NUMBER) {
                io::eprintn("error: not a number");
                return 1;
        }
        io::printfn("result: %f", res.as.number);
        return 0;
}

fn char[]! __malloc__(usz size) => mem::alloc_array(char, size);
