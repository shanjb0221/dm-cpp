#include <napi.h>
using namespace Napi;

#include<map>
#include<stack>
#include<vector>
#include<cstdio>
#include<string>
#include<cstring>
#include<cstdlib>
#include<iomanip>
#include<sstream>


namespace Common{
std::stringstream serr,sout;
inline void clear(){serr.str(""),sout.str("");}
} // namespace Common


namespace shanjb20{
using namespace Common;
using namespace std;
typedef std::map<string,int> dictionary;
dictionary dic;

struct node {
	int type,value;
	node *son[2];
	inline node(int t,int v):type(t),value(v) { son[0]=son[1]=NULL; };
};

class nodeStack {
		std::stack<node*> stk;
	public:
		inline bool empty() { return stk.empty(); }
		inline node* pop() {
			if(stk.empty())throw(1001);
			node* res=stk.top();
			stk.pop();
			return res;
		}
		inline node* top() {
			if(stk.empty())throw(1001);
			return stk.top();
		}
		inline void push(node* x) { stk.push(x); }
		inline void clear() { while(!empty())pop(); } 
} stk;

typedef std::vector<node*> group;
std::vector<string> rdic;
int tot=0;
int idx=0;

namespace output{
// Default Config
bool Index=true;
bool LaTex=true;
bool Trace=true;
bool Infix=true;

// op: 0 [=>] 1 ! 2 & 3 | 4 > 5 =  6 ( 7 ) 8 [s=>]
inline string getOp(int id) {
	switch(id){
		case 0: 	return LaTex?"\\Rightarrow ":"=>";
		case '!':
		case 1: 	return LaTex?"\\neg ":"!";
		case '&':
		case 2: 	return LaTex?"\\wedge ":"&";
		case '|':
		case 3: 	return LaTex?"\\vee ":"|";
		case '>':
		case 4: 	return LaTex?"\\rightarrow ":">";
		case '=':
		case 5:		return LaTex?"\\leftrightarrow ":"=";
		case '(':
		case 6:		return LaTex?"\\left( ":"(";
		case ')':
		case 7:		return LaTex?"\\right) ":")";
		case 's':
		case 8: 	return LaTex?"\\mathop{\\Rightarrow}\\limits^{s} ":"s=>";
		case '{':	return LaTex?"\\left\\{ ":"{";
		case '}':	return LaTex?"\\right\\} ":"}";
		case '[':	return LaTex?"\\left[ ":"[";
		case ']':	return LaTex?"\\right] ":"]";
		case '\t':	return LaTex?"|":"\t";
		default: throw(1004);
	}
	return "";
}
inline string getName(int id) {
	return LaTex?"\\text{"+rdic[id-1]+"} ":rdic[id-1];
}
inline bool check(node* p,node* pnt){
	if(pnt==NULL)return false;
	if(p==pnt->son[0])return pnt->value<p->value;
	return pnt->value<=p->value;
}
inline void print(node* p,node* pnt=NULL) {
	if(Infix){
		if(p->type==0) {
			sout<<getName(p->value);
		} else {
			if(check(p,pnt))sout<<getOp('(');
			if(p->son[0])print(p->son[0],p);
			sout<<getOp(p->value);
			if(p->son[1])print(p->son[1],p);
			if(check(p,pnt))sout<<getOp(')');
		}
	} else {
		if(p->type==0) {
			//sout<<"[id:"<<p->value<<";name:"<<getName(p->value)<<"]";
			sout<<getName(p->value)<<" ";
		} else {
			if(p->son[0])print(p->son[0]);
			//sout<<"[op:"<<getOp(p->value)<<"]";
			if(p->son[1])print(p->son[1]);
			sout<<getOp(p->value)<<" ";
		}
	}
}
inline void print(group arr) {
	sout<<getOp('{');
	for(auto i:arr) { print(i);sout<<"; "; }
	sout<<getOp('}');
}
} // namespace output
using namespace output;

inline int isOp(int ch) {
	switch(ch) {
		case '~':
		case '!': return 1;
		case '&': return 2;
		case '|': return 3;
		case '>': return 4;
		case '=': return 5;
		case '(': return 6;
		case ')': return 7;
	}
	return 0;
}
inline int getId(string str) {
	if(!dic[str]) {
		rdic.push_back(str);
		dic[str]=++tot;
		return tot;
	}
	return dic[str];
}

group getInfixExpression(string str) {
	group res;
	for(int i=0,j,_siz=str.size(); i<_siz;) {
		if(isOp(str[i])) {
			res.push_back(new node(1,isOp(str[i])));
			++i;
		} else {
			j=1;
			while(i+j<_siz&&!isOp(str[i+j]))++j;
			res.push_back(new node(0,getId(str.substr(i,j))));
			i+=j;
		}
	}
	return res;
}

group getPostfixExpression(group ie) {
	group res;
	int operandCount=0;
	node* tmp;
	stk.clear(),stk.push(new node(1,6));
	for(auto i:ie)
		if(i->type==0){
			operandCount++;
			if(operandCount>1)throw(1000);
			res.push_back(i);
		} else
			switch(i->value) {
				case 7: // )
					while(stk.top()->value!=6) {
						tmp=stk.pop();
						res.push_back(tmp);
					}
					stk.pop();
					if(stk.empty())throw(1000);
					break;
				case 6:	// (
				case 1: // ~
					if(operandCount!=0)throw(1000);
					stk.push(i);
					break;
				default:
					--operandCount;
					if(operandCount<0)throw(1000); 
					while(i->value>=stk.top()->value) {
						tmp=stk.pop();
						res.push_back(tmp);
					}
					stk.push(i);
			}
	while(stk.top()->value!=6) {
		tmp=stk.pop();
		res.push_back(tmp);
	}
	stk.pop();
	if(!stk.empty())throw(1000);
	if(operandCount!=1)throw(1000);
	return res;
}

node* buildTree(group pe) {
	stk.clear(); 
	for(auto i:pe)
		if(i->type==0)stk.push(i);
		else {
			i->son[1]=stk.pop();
			if(i->value!=1)i->son[0]=stk.pop();
			stk.push(i);
		}
	return stk.pop();
}

inline void initialize(){
  Common::clear();
  dic.clear();
  rdic.clear();
  stk.clear();
  tot=idx=0;
}

namespace wh{
inline void newline(){
	if(LaTex)sout<<"\\\\"<<endl;
	else sout<<endl;
}
inline bool check(group L,group R) {
	for(auto l:L)
		if(l->type==0)
			for(auto r:R)
				if(r->type==0&&l->value==r->value)return 1;
	return false;
}

bool solve(group L,group R,int pnt=0) {
	int id=++idx;
	if(idx>=100)throw(1002);
	bool isTrue=check(L,R);
	if(Trace){
		if(Index){
			sout<<getOp('(')<<setw(2)<<id<<getOp(')')<<" &"[LaTex]<<getOp('[')<<setw(2)<<pnt<<getOp(']')<<" &"[LaTex];
		}
		print(L);sout<<" &"[LaTex]<<getOp('s');print(R);
		if(isTrue)sout<<" &"[LaTex]<<"(true)",newline();
	}
	if(isTrue)return true;
	node* t=NULL;
	group TL,TR;
	for(auto i=L.begin(); i!=L.end(); ++i)
		if((*i)->type==1) {
			t=*i;
			L.erase(i);
			break;
		}
	if(t) {
		if(Trace){
			sout<<" &"[LaTex]<<getOp('(')<<getOp(t->value)<<getOp(0)<<getOp(')');newline();
		}
		switch(t->value) {
			case 1:
				R.push_back(t->son[1]);
				return solve(L,R,id);
			case 2:
				L.push_back(t->son[0]);
				L.push_back(t->son[1]);
				return solve(L,R,id);
			case 3:
				TL=L;
				L.push_back(t->son[0]);
				TL.push_back(t->son[1]);
				return solve(L,R,id)&&solve(TL,R,id);
			case 4:
				TL=L,TR=R;
				TR.push_back(t->son[0]);
				TL.push_back(t->son[1]);
				return solve(L,TR,id)&&solve(TL,R,id);
			case 5:
				TL=L,TR=R;
				TR.push_back(t->son[0]);
				TR.push_back(t->son[1]);
				TL.push_back(t->son[0]);
				TL.push_back(t->son[1]);
				return solve(L,TR,id)&&solve(TL,R,id);
		}
	}
	for(auto i=R.begin(); i!=R.end(); ++i)
		if((*i)->type==1) {
			t=*i;
			R.erase(i);
			break;
		}
	if(t) {
		if(Trace){
			sout<<" &"[LaTex]<<getOp('(')<<getOp(0)<<getOp(t->value)<<getOp(')');newline();
		}
		switch(t->value) {
			case 1:
				L.push_back(t->son[1]);
				return solve(L,R,id);
			case 3:
				R.push_back(t->son[0]);
				R.push_back(t->son[1]);
				return solve(L,R,id);
			case 2:
				TR=R;
				R.push_back(t->son[0]);
				TR.push_back(t->son[1]);
				return solve(L,R,id)&&solve(L,TR,id);
			case 4:
				L.push_back(t->son[0]);
				R.push_back(t->son[1]);
				return solve(L,R,id);
			case 5:
				TL=L,TR=R;
				L.push_back(t->son[0]);
				R.push_back(t->son[0]);
				TL.push_back(t->son[1]);
				TR.push_back(t->son[1]);
				return solve(L,TR,id)&&solve(TL,R,id);
		}
	}
	if(Trace){
		sout<<" &"[LaTex]<<"(false)";newline();
	}
	return false;
}
string main(string str) {
 	initialize();
	try {
		if(str.size()>50)throw(1005);
		if(Trace&&LaTex)sout<<"$$\n\\begin{aligned}"<<endl;
		group ie=getInfixExpression(str);
		group pe=getPostfixExpression(ie);
		node* root=buildTree(pe);
		bool res=solve(group(),group(1,root));
		if(Trace&&LaTex)sout<<"\\end{aligned}\n$$"<<endl;
		sout<<"[RESULT] "<<boolalpha<<res<<endl;
	} catch(int e) {
		serr<<"[ERROR] ("<<e<<") : ";
		switch(e) {
			case 1000:
				serr<<"Invalid Expression";
				break;
			case 1002:
				serr<<"Killed : Too Many Steps!";
				break;
			case 1005:
				serr<<"Killed : Too Long Input!";
				break;
			default:
				serr<<"Unknown Error";
		}
		serr<<endl;
		return serr.str();
	} catch(...) {
		serr<<"[ERROR] () : Unknown Error"<<endl;
		return serr.str();
	}
  	return sout.str();
}
} // namespace wh

namespace tt{
inline void newline(){
	if(LaTex)sout<<"|"<<endl;
	else sout<<endl;
}
inline bool calc(node* p,int st){
	if(p->type==0)return (st>>(p->value-1))&1;
	switch(p->value){
		case 1:
			return !calc(p->son[1],st);
		case 2:
			return calc(p->son[0],st)&&calc(p->son[1],st);
		case 3:
			return calc(p->son[0],st)||calc(p->son[1],st);
		case 4:
			return (!calc(p->son[0],st))||calc(p->son[1],st);
		case 5:
			return calc(p->son[0],st)==calc(p->son[1],st);
	}
	return false;
}
inline string getTruthTable(node* p){
	string res="";
	if(tot>6)throw(1003);
	int cnt=1<<tot;
	if(Trace){
		if(LaTex)sout<<"|";
		if(Index)sout<<"id"<<getOp('\t');
		for(int i=1;i<=tot;++i)
			sout<<(LaTex?"$":"")<<getName(i)<<(LaTex?"$":"")<<getOp('\t');
		sout<<(LaTex?"$":"");print(p);sout<<(LaTex?"$":"");newline();
		if(LaTex){
			if(Index)sout<<"|:-:";
			for(int i=1;i<=tot;++i)sout<<"|:-:";
			sout<<"|:-:";
			newline();
		}
	}
	for(int st=0;st<cnt;++st){
		bool ret=calc(p,st);
		if(Trace){
			if(LaTex)sout<<"|";
			if(Index)sout<<setw(2)<<st<<getOp('\t');
			for(int i=1;i<=tot;++i)
				sout<<"FT"[(st>>(i-1))&1]<<getOp('\t');
			sout<<"FT"[ret];newline();
		}
		res.push_back("FT"[ret]);
	}
	return res;
}
string main(string str) {
	initialize();
	try {
		if(str.size()>50)throw(1005);
		group ie=getInfixExpression(str);
		group pe=getPostfixExpression(ie);
		node* root=buildTree(pe);
		string res=getTruthTable(root);
		sout<<"[RESULT] "<<res<<endl;
	} catch(int e) {
		serr<<"[ERROR] ("<<e<<") : ";
		switch(e) {
			case 1000:
				serr<<"Invalid Expression";
				break;
			case 1003:
				serr<<"Killed : Too Many Variables!";
				break;
			case 1005:
				serr<<"Killed : Too Long Input!";
				break;
			default:
				serr<<"Unknown Error";
		}
		serr<<endl;
		return serr.str();
	} catch(...) {
		serr<<"[ERROR] () : Unknown Error"<<endl;
		return serr.str();
	}
	return sout.str();
}
} // namespace tt

String whMain(const CallbackInfo& info) {
	string str = info[0].As<String>().Utf8Value();
	string res = wh::main(str);
  	return String::New(info.Env(), res);
}
String ttMain(const CallbackInfo& info) {
	string str = info[0].As<String>().Utf8Value();
	string res = tt::main(str);
  	return String::New(info.Env(), res);
}
} // namespace shanjb20

Napi::Object Init(Env env, Object exports) {
  exports.Set("shanjb20_wh", Function::New(env, shanjb20::whMain));
  exports.Set("shanjb20_tt", Function::New(env, shanjb20::ttMain));
  return exports;
}
NODE_API_MODULE(addon, Init)