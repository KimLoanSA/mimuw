#ifndef _S_I_DFA_H
#define _S_I_DFA_H

#define DFAIOCRESET   _IO('a', 1)
#define DFAIOCADD     _IOW('a', 2, char[3])
#define DFAIOCACCEPT  _IOW('a', 3, char)
#define DFAIOCREJECT  _IOW('a', 4, char)

#endif /* _S_I_DFA_H */