#include <napi.h>

using namespace Napi;

#define Infix
#define Trace
//#define Index
#include<map>
#include<stack>
#include<cstdio>
#include<string>
#include<vector>
#include<cstring>
#include<cstdlib>
#include<iomanip>
#include<sstream>
using namespace std;
stringstream sout;
typedef map<string,int> dictionary;
dictionary dic;
struct node {
	int type,value;
	node *son[2];
	inline node(int t,int v):type(t),value(v) { son[0]=son[1]=NULL; };
};
typedef vector<node*> group;
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
inline char getOp(int id) { return " ~&|>=()"[id];}
int tot=0;
vector<string> rdic;
inline int getId(string str) {
	if(!dic[str]) {
		rdic.push_back(str);
		dic[str]=++tot;
		return tot;
	}
	return dic[str];
}
inline string getName(int id) { return rdic[id-1]; }
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
class nodeStack {
		stack<node*> stk;
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
inline bool check(group L,group R) {
	for(auto l:L)
		if(l->type==0)
			for(auto r:R)
				if(r->type==0&&l->value==r->value)return 1;
	return false;
}
#ifdef Trace
inline void print(node* p,node* pnt=NULL) {
#ifdef Infix
	if(p->type==0) {
		//sout<<"[id:"<<p->value<<";name:"<<getName(p->value)<<"]";
		sout<<getName(p->value);
	} else {
		if(pnt!=NULL&&pnt->value<=p->value)sout<<"(";
		if(p->son[0])print(p->son[0],p);
		//sout<<"[op:"<<getOp(p->value)<<"]";
		sout<<getOp(p->value);
		if(p->son[1])print(p->son[1],p);
		if(pnt!=NULL&&pnt->value<=p->value)sout<<")";
	}
#else
	if(p->type==0) {
		//sout<<"[id:"<<p->value<<";name:"<<getName(p->value)<<"]";
		sout<<getName(p->value)<<" ";
	} else {
		if(p->son[0])print(p->son[0]);
		//sout<<"[op:"<<getOp(p->value)<<"]";
		if(p->son[1])print(p->son[1]);
		sout<<getOp(p->value)<<" ";
	}
#endif
}
inline void print(group arr) {
	sout<<"{";
	for(auto i:arr) { print(i);sout<<";"; }
	sout<<"}";
}
#endif
int idx=0;
bool solve(group L,group R,int pnt=0) {
	int id=++idx;
	if(idx>=100)throw(1002);
	bool isTrue=check(L,R);
#ifdef Trace
#ifdef Index
	sout<<"("<<setw(2)<<id<<") ["<<setw(2)<<pnt<<"] ";
#endif
	print(L);sout<<" s=> ";print(R);
	if(isTrue)sout<<" <true>"<<endl;
#endif
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
#ifdef Trace
		sout<<" (["<<getOp(t->value)<<"]=>)"<<endl;
#endif
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
#ifdef Trace
		sout<<" (=>["<<getOp(t->value)<<"])"<<endl;
#endif
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
#ifdef Trace
	sout<<" <false>"<<endl;
#endif
	return false;
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
#ifdef Trace
#ifdef Index
	sout<<"id"<<"\t";
#endif
	for(int i=1;i<=tot;++i)
		sout<<getName(i)<<"\t";
	print(p);sout<<endl;
#endif
	for(int st=0;st<cnt;++st){
		bool ret=calc(p,st);
#ifdef Trace
#ifdef Index
		sout<<setw(2)<<st<<"\t";
#endif
		for(int i=1;i<=tot;++i)
			sout<<"FT"[(st>>(i-1))&1]<<"\t";
		sout<<"FT"[ret]<<endl;
#endif
		res.push_back("FT"[ret]);
	}
	return res;
}
inline void initialize(){
  sout.str("");
  dic.clear();
  rdic.clear();
  stk.clear();
  tot=idx=0;
}
String wanghaoMain(const CallbackInfo& info) {
  initialize();
  string str = info[0].As<String>().Utf8Value();
	try {
		group ie=getInfixExpression(str);
		group pe=getPostfixExpression(ie);
		node* root=buildTree(pe);
		bool res=solve(group(),group(1,root));
		sout<<"[RESULT] <"<<boolalpha<<res<<">"<<endl;
	} catch(int e) {
		sout<<"[ERROR] ("<<e<<") : ";
		switch(e) {
			case 1000:
				sout<<"Invalid Expression";
				break;
			case 1002:
				sout<<"Killed : Too Many Steps!";
				break;
			default:
				sout<<"Unknown Error";
		}
		sout<<endl;
	} catch(...) {
		sout<<"[ERROR] () : Unknown Error"<<endl;
	}
  return String::New(info.Env(), sout.str());
}
String truthTableMain(const CallbackInfo& info) {
	initialize();
	string str = info[0].As<String>().Utf8Value();
	try {
		group ie=getInfixExpression(str);
		group pe=getPostfixExpression(ie);
		node* root=buildTree(pe);
		string res=getTruthTable(root);
		sout<<"[RESULT] <"<<res<<">"<<endl;
	} catch(int e) {
		sout<<"[ERROR] ("<<e<<") : ";
		switch(e) {
			case 1000:
				sout<<"Invalid Expression";
				break;
			case 1003:
				sout<<"Killed : Too Many Variables!";
				break;
			default:
				sout<<"Unknown Error";
		}
		sout<<endl;
	} catch(...) {
		sout<<"[ERROR] () : Unknown Error"<<endl;
	}
	return String::New(info.Env(), sout.str());
}


Napi::Object  Init(Env env, Object exports) {
  exports.Set("wanghao", Function::New(env, wanghaoMain));
  exports.Set("truthTable", Function::New(env, truthTableMain));
  return exports;
}
NODE_API_MODULE(addon, Init)