// adopted from http://en.cppreference.com/w/cpp/atomic/atomic/compare_exchange
#include <atomic>
#include <thread>
#include <iostream>

template<typename T>
struct node
{
    T data;
    node* next;
    node(const T& data) : data(data), next(nullptr) {}
};
 
template<typename T>
class stack
{
    std::atomic<node<T>*> head;
 public:
    void push(const T& data)
    {
      node<T>* new_node = new node<T>(data);
      // thread-unsafe code:
      // new_node->next = head;
      // head = new_node;

      // put the current value of head into new_node->next
      new_node->next = head.load();
 
      // now make new_node the new head, but if the head
      // is no longer what's stored in new_node->next
      // (some other thread must have inserted a node just now)
      // then put that new head into new_node->next and try again
      while(!head.compare_exchange_weak(new_node->next, new_node))
          ; // the body of the loop is empty
    }
    node<T>* get_head() {
        return head;
    }
};

stack<int> s;

const int counter = 1000000;

void f() {
    for (int i = 0; i < counter; i++)
        s.push(i);
}

int main()
{
    std::thread t1{f};
    std::thread t2{f};
    t1.join();
    t2.join();
    int node_count = 0;
    for (node<int>* n = s.get_head(); n != nullptr; n = n->next)
        node_count++;
    std::cout << "node count: "<<node_count<<std::endl;
}
