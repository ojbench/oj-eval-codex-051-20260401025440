#include <bits/stdc++.h>
using namespace std;

// Simple assembler: parse .dmp and emit .tis memory image.
// Supports: labels, ., msubleq/rsubleq/ldorst, implicit args expansion,
// expressions with +, -, unary -, parentheses, ids, numbers, and '?', and id: prefixes.

struct Expr {
    // AST for expressions
    enum Kind { K_NUM, K_LABEL, K_QMARK, K_NEG, K_ADD, K_SUB, K_GROUP } kind;
    long long val; // for K_NUM
    string name;   // for K_LABEL
    shared_ptr<Expr> a, b; // for unary/binary
};

struct Item { // an item occupies memory
    vector<string> leadingLabels; // labels before expression
    shared_ptr<Expr> expr;
    int memIndex = -1; // resolved address
};

struct Instr {
    // opcode and up to 3 items, or data-only '.' which contributes only items
    enum Type { T_MSUBLEQ, T_RSUBLEQ, T_LDORST, T_DOT } type;
    vector<string> leadingLabels; // labels before opcode
    vector<Item> items; // each will consume memory cell
    int opcodeIndex = -1; // memory index where opcode is written (if not DOT)
};

static bool isIdStart(char c){ return c=='_' || isalpha((unsigned char)c); }
static bool isIdChar(char c){ return c=='_' || isalnum((unsigned char)c); }

struct Token { string kind; string text; };

struct Lexer {
    string s; size_t i=0; int line=1;
    Lexer(const string& in):s(in){}
    bool debug = (getenv("DBG")!=nullptr);
    void skipSpaces(){
        while(i<s.size()){
            char c=s[i];
            if(c=='\n'){ line++; i++; }
            else if(isspace((unsigned char)c)) i++;
            else if(c=='/' && i+1<s.size() && s[i+1]=='/'){
                while(i<s.size() && s[i]!='\n') i++;
            } else break;
        }
    }
    Token next(){
        skipSpaces();
        if(i>=s.size()) return {"eof",""};
        char c=s[i];
        // punctuation
        string one = string(1,c);
        if(string(":;()+-.,").find(c)!=string::npos){ i++; return {one,one}; }
        if(c=='?'){ i++; return {"?","?"}; }
        if(isdigit((unsigned char)c) || (c=='-' && i+1<s.size() && isdigit((unsigned char)s[i+1]))){
            // number literal (allow leading - as part of number when followed by digit)
            size_t j=i; if(s[j]=='-') j++;
            while(j<s.size() && isdigit((unsigned char)s[j])) j++;
            string t=s.substr(i,j-i); i=j; return {"num",t};
        }
        if(isIdStart(c)){
            size_t j=i; while(j<s.size() && isIdChar(s[j])) j++;
            string t=s.substr(i,j-i); i=j; return {"id",t};
        }
        // unknown char; consume
        i++; return {one,one};
    }
    size_t mark() const { return i; }
    void reset(size_t p){ i=p; }
};

struct Parser {
    Lexer lex;
    Token tok;
    Parser(const string& in):lex(in){ tok=lex.next(); }
    void adv(){ tok=lex.next(); if(getenv("DBG")) cerr<<"TOK kind='"<<tok.kind<<"' text='"<<tok.text<<"'\n"; }
    bool accept(const string& k){ if(tok.kind==k){ if(getenv("DBG")) cerr<<"ACCEPT '"<<k<<"'\n"; adv(); return true;} return false; }
    Token peek(){ size_t m=lex.mark(); int ln=lex.line; Token t=lex.next(); lex.reset(m); lex.line=ln; return t; }
    void expect(const string& k){ if(!accept(k)) throw runtime_error("Expected '"+k+"'"); }

    shared_ptr<Expr> parseExpression(){ return parseAddSub(); }
    shared_ptr<Expr> parsePrimary(){
        if(tok.kind=="num"){ auto e=make_shared<Expr>(); e->kind=Expr::K_NUM; e->val=stoll(tok.text); adv(); return e; }
        if(tok.kind=="id"){ auto e=make_shared<Expr>(); e->kind=Expr::K_LABEL; e->name=tok.text; adv(); return e; }
        if(tok.kind=="?"){ auto e=make_shared<Expr>(); e->kind=Expr::K_QMARK; adv(); return e; }
        if(accept("(")){
            auto in = parseExpression();
            expect(")");
            auto e=make_shared<Expr>(); e->kind=Expr::K_GROUP; e->a=in; return e;
        }
        if(accept("-")){
            auto sub = parsePrimary();
            auto e=make_shared<Expr>(); e->kind=Expr::K_NEG; e->a=sub; return e;
        }
        if(accept("+")){
            // unary plus: no-op
            return parsePrimary();
        }
        throw runtime_error("Unexpected token in expression: "+tok.kind+"/"+tok.text);
    }
    shared_ptr<Expr> parseAddSub(){
        auto e = parsePrimary();
        while(tok.kind=="+" || tok.kind=="-"){
            string op=tok.kind; adv();
            auto r = parsePrimary();
            auto ne=make_shared<Expr>(); ne->kind = (op=="+")?Expr::K_ADD:Expr::K_SUB; ne->a=e; ne->b=r; e=ne;
        }
        return e;
    }

    vector<Instr> parseProgram(){
        vector<Instr> prog;
        while(tok.kind!="eof"){
            // collect leading labels (id ':')*
            vector<string> leading;
            while(tok.kind=="id" && peek().kind==":"){
                string name=tok.text; adv(); // id
                expect(":");
                leading.push_back(name);
            }
            if(tok.kind=="eof") break;
            // opcode: '.', msubleq, rsubleq, ldorst
            Instr ins; ins.leadingLabels = leading;
            if(getenv("DBG")) cerr<<"OPC TOK kind='"<<tok.kind<<"' text='"<<tok.text<<"'\n";
            if(tok.kind=="."){
                if(getenv("DBG")) cerr<<"OPC is DOT, adv to items\n";
                ins.type=Instr::T_DOT; adv();
            } else if(tok.kind=="id"){
                string op=tok.text; adv();
                if(op=="msubleq") ins.type=Instr::T_MSUBLEQ;
                else if(op=="rsubleq") ins.type=Instr::T_RSUBLEQ;
                else if(op=="ldorst") ins.type=Instr::T_LDORST;
                else throw runtime_error("Unknown opcode: "+op);
            } else {
                throw runtime_error("Expected opcode or '.'");
            }
            // parse items until ';'
            if(getenv("DBG")) cerr<<"Begin items; current tok kind='"<<tok.kind<<"' text='"<<tok.text<<"'\n";
            while(!accept(";")){
                // allow comma-separated or space-separated; specs imply space separated items, but accept ',' as well
                // handle (label:)* expression
                Item it; vector<string> labs;
                while(tok.kind=="id" && peek().kind==":"){
                    string nm=tok.text; adv(); expect(":"); labs.push_back(nm);
                }
                it.leadingLabels = labs;
                if(getenv("DBG")) cerr<<"Parse expr start: tok kind='"<<tok.kind<<"' text='"<<tok.text<<"'\n";
                it.expr = parseExpression();
                ins.items.push_back(it);
                // optional comma
                accept(",");
            }
            prog.push_back(ins);
        }
        return prog;
    }
};

struct Assembler {
    vector<long long> mem; // memory image
    unordered_map<string,long long> labelAddr; // label -> memory index

    static long long evalExpr(const shared_ptr<Expr>& e, long long itemAddr){
        if(!e) return 0;
        switch(e->kind){
            case Expr::K_NUM: return e->val;
            case Expr::K_LABEL: return 0; // resolved later
            case Expr::K_QMARK: return itemAddr+1; // '?' equals address+1
            case Expr::K_NEG: return -evalExpr(e->a, itemAddr);
            case Expr::K_ADD: return evalExpr(e->a,itemAddr)+evalExpr(e->b,itemAddr);
            case Expr::K_SUB: return evalExpr(e->a,itemAddr)-evalExpr(e->b,itemAddr);
            case Expr::K_GROUP: return evalExpr(e->a,itemAddr);
        }
        return 0;
    }

    static void collectLabelsInExpr(const shared_ptr<Expr>& e, vector<string>& out){
        if(!e) return;
        if(e->kind==Expr::K_LABEL){ out.push_back(e->name); return; }
        collectLabelsInExpr(e->a,out); collectLabelsInExpr(e->b,out);
    }

    void firstPass(vector<Instr>& prog){
        long long pc=0; // memory index
        for(auto &ins: prog){
            // bind leading labels to current pc (opcode for real instructions, first item for '.')
            for(const auto& lb: ins.leadingLabels){ labelAddr[lb]=pc; }
            if(ins.type==Instr::T_DOT){
                // items occupy memory, bind their internal labels to their addresses
                for(auto &it: ins.items){
                    for(const auto& lb: it.leadingLabels){ labelAddr[lb]=pc; }
                    it.memIndex = (int)pc;
                    pc += 1;
                }
            } else {
                // real instruction: opcode + up to 3 items (after expansion rules)
                ins.opcodeIndex = (int)pc; pc += 1;
                // expand implicit args for msubleq and rsubleq
                if((ins.type==Instr::T_MSUBLEQ || ins.type==Instr::T_RSUBLEQ) && ins.items.size()==1){
                    // msubleq A; => msubleq A A ?
                    Item A = ins.items[0];
                    Item B = A; Item C; C.expr = make_shared<Expr>(); C.expr->kind=Expr::K_QMARK;
                    ins.items = {A,B,C};
                } else if((ins.type==Instr::T_MSUBLEQ || ins.type==Instr::T_RSUBLEQ) && ins.items.size()==2){
                    // msubleq A B; => msubleq A B ?
                    Item C; C.expr = make_shared<Expr>(); C.expr->kind=Expr::K_QMARK;
                    ins.items.push_back(C);
                }
                // assign addresses to items
                for(auto &it: ins.items){
                    for(const auto& lb: it.leadingLabels){ labelAddr[lb]=pc; }
                    it.memIndex = (int)pc;
                    pc += 1;
                }
            }
        }
        // done
    }

    long long resolveExpr(const shared_ptr<Expr>& e, long long itemAddr){
        if(!e) return 0;
        switch(e->kind){
            case Expr::K_NUM: return e->val;
            case Expr::K_LABEL: {
                auto it = labelAddr.find(e->name);
                if(it==labelAddr.end()) throw runtime_error("Undefined label: "+e->name);
                return it->second;
            }
            case Expr::K_QMARK: return itemAddr+1;
            case Expr::K_NEG: return -resolveExpr(e->a,itemAddr);
            case Expr::K_ADD: return resolveExpr(e->a,itemAddr)+resolveExpr(e->b,itemAddr);
            case Expr::K_SUB: return resolveExpr(e->a,itemAddr)-resolveExpr(e->b,itemAddr);
            case Expr::K_GROUP: return resolveExpr(e->a,itemAddr);
        }
        return 0;
    }

    void secondPass(vector<Instr>& prog){
        // determine total size
        long long maxIdx = 0;
        for(auto &ins: prog){
            if(ins.type==Instr::T_DOT){
                for(auto &it: ins.items) maxIdx = max(maxIdx, (long long)it.memIndex);
            } else {
                maxIdx = max(maxIdx, (long long)ins.opcodeIndex);
                for(auto &it: ins.items) maxIdx = max(maxIdx, (long long)it.memIndex);
            }
        }
        mem.assign(maxIdx+1, 0);
        for(auto &ins: prog){
            if(ins.type==Instr::T_DOT){
                for(auto &it: ins.items){
                    long long v = resolveExpr(it.expr, it.memIndex);
                    mem[it.memIndex] = v;
                }
            } else {
                int opcode = (ins.type==Instr::T_MSUBLEQ)?0: (ins.type==Instr::T_RSUBLEQ?1:2);
                mem[ins.opcodeIndex] = opcode;
                for(auto &it: ins.items){
                    long long v = resolveExpr(it.expr, it.memIndex);
                    mem[it.memIndex] = v;
                }
            }
        }
    }
};

int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    // read entire stdin
    string input, line;
    string tmp;
    {
        std::ostringstream oss; oss<<cin.rdbuf(); input = oss.str();
    }
    try{
        Parser p(input);
        auto prog = p.parseProgram();
        Assembler a; a.firstPass(prog); a.secondPass(prog);
        // output: numbers separated by space and newline every 4? Sample shows lines grouping by 4 elements but actually arbitrary.
        // We'll print all numbers separated by space and newline at end.
        for(size_t i=0;i<a.mem.size();++i){
            cout<<a.mem[i];
            if((i+1)%4==0) cout<<" \n";
            else if(i+1<a.mem.size()) cout<<" ";
        }
    }catch(const exception& e){
        cerr << "ERR: " << e.what() << "\n";
        return 0;
    }
    return 0;
}
