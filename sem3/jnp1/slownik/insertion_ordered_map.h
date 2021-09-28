#ifndef INSERTION_ORDERED_MAP_H
#define INSERTION_ORDERED_MAP_H

#include <unordered_map>
#include <iostream>
#include <utility>
#include <list>
#include <memory>

class lookup_error : public std::exception {
};

template<class K, class V, class Hash = std::hash<K>>
class insertion_ordered_map {
private:

  using _list_K_V_iterator = typename std::list<std::pair<K, V>>::iterator;
  using _list_K_V = std::list<std::pair<K, V>>;
  using _map_K_iterator = std::unordered_map<K, _list_K_V_iterator, Hash>;


  std::shared_ptr<_map_K_iterator> map_ptr;
  std::shared_ptr<_list_K_V> list_ptr;
  std::shared_ptr<bool> ref_given;

  _list_K_V_iterator get_last_list_iterator() {
    _list_K_V_iterator res_it = list_ptr->end();

    if (list_ptr->size() > 0) {
      res_it--;
    }

    return res_it;
  }

  void copy();

  void copy_if_ref_given();

  void insert_iterators_to_map();

  V &get_element(K const &k);

  void assign_pointers(std::shared_ptr<_list_K_V> lp,
    std::shared_ptr<_map_K_iterator> mp);

  bool insert_if_exist(const K &k);

  bool insert_if_does_not_exist(const K &k, const V &v);

  void insert_last_elem_to_map();

  void insert_this_map_to_new_map(_map_K_iterator &map_to_insert,
    _list_K_V &list_to_insert) const;

  void insert_other_map_to_new_map(const insertion_ordered_map &other,
    _map_K_iterator &map_to_insert, _list_K_V &list_to_insert) const;

public:

  class iterator {
  private:

    using _list_const_iterator =
    typename std::list<std::pair<K, V>>::const_iterator;

    _list_const_iterator it;

    std::pair<K, V> value;

    void update_value() {
      this->value = *(this->it);
    }

  public:

    iterator() {}

    iterator(const iterator &iterator) {
      this->it = _list_const_iterator(iterator->it);

      this->update_value();
    }

    iterator(typename std::list<std::pair<K, V>>::const_iterator list_it) {
      this->it = list_it;
      this->update_value();
    }

    iterator &operator++() {
      this->it++;
      this->update_value();

      return *this;
    }

    iterator &operator=(iterator other) {
      this->it = other->it;
      this->update_value();

      return *this;
    }

    bool operator==(const iterator &iterator) {
      return this->it == iterator.it;
    }

    bool operator!=(const iterator &iterator) {
      return !(*this == iterator);
    }

    const std::pair<K, V> &operator*() {
      return *it;
    }

    std::pair<K, V> *operator->() {
      return &value;
    }
  };


  iterator begin() const {
    return iterator(list_ptr->begin());
  }

  iterator end() const {
    return iterator(list_ptr->end());
  }

  insertion_ordered_map();

  insertion_ordered_map(insertion_ordered_map const &other);

  insertion_ordered_map(insertion_ordered_map &&other);

  insertion_ordered_map &operator=(insertion_ordered_map other);

  bool insert(K const &k, V const &v);

  void erase(K const &k);

  size_t size() const;

  bool empty() const;

  void clear();

  void merge(insertion_ordered_map const &other);

  V &at(K const &k);

  V const &at(K const &k) const;

  V &operator[](K const &k);

  bool contains(K const &k) const;

  void printList();
};

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::
assign_pointers(std::shared_ptr<_list_K_V> lp,
  std::shared_ptr<_map_K_iterator> mp) {
  list_ptr = lp;
  map_ptr = mp;
}

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::copy() {
  if (map_ptr.use_count() > 1) {
    list_ptr = std::make_shared<_list_K_V>(*list_ptr);
    map_ptr = std::make_shared<_map_K_iterator>();
    ref_given = std::make_shared<bool>(false);

    this->insert_iterators_to_map();
  }
};

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::copy_if_ref_given() {
  if (*ref_given) {
    list_ptr = std::make_shared<_list_K_V>(*list_ptr);
    map_ptr = std::make_shared<_map_K_iterator>();
    ref_given = std::make_shared<bool>(false);

    this->insert_iterators_to_map();
  }
};

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::insert_iterators_to_map() {
  for (_list_K_V_iterator list_it = list_ptr->begin();
    list_it != list_ptr->end(); list_it++) {
    map_ptr->insert({list_it->first, list_it});
  }
}

template<class K, class V, class Hash>
V &insertion_ordered_map<K, V, Hash>::get_element(const K &k) {
  return this->map_ptr->at(k)->second;
}

template<class K, class V, class Hash>
insertion_ordered_map<K, V, Hash>::insertion_ordered_map() {
  this->list_ptr = std::make_shared<_list_K_V>();
  this->map_ptr = std::make_shared<_map_K_iterator>();
  this->ref_given = std::make_shared<bool>(false);
}

template<class K, class V, class Hash>
insertion_ordered_map<K, V, Hash>::
insertion_ordered_map(insertion_ordered_map const &other) :
  map_ptr{other.map_ptr}, list_ptr{other.list_ptr}, ref_given{other.ref_given} {
  this->copy_if_ref_given();
}

template<class K, class V, class Hash>
insertion_ordered_map<K, V, Hash>::
insertion_ordered_map(insertion_ordered_map &&other) :
  map_ptr{other.map_ptr}, list_ptr{other.list_ptr}, ref_given{other.ref_given} {

  other.list_ptr = std::make_shared<_list_K_V>();
  other.map_ptr = std::make_shared<_map_K_iterator>();

  this->copy_if_ref_given();
}

// Przeciążenie operatora przypisania
template<class K, class V, class Hash>
insertion_ordered_map<K, V, Hash> &
insertion_ordered_map<K, V, Hash>::operator=(insertion_ordered_map other) {
  this->assign_pointers(other.list_ptr, other.map_ptr);
  ref_given = other.ref_given;
  this->copy_if_ref_given();

  return *this;
}

template<class K, class V, class Hash>
bool insertion_ordered_map<K, V, Hash>::insert(K const &k, V const &v) {
  this->copy();

  // Jest taki klucz w mapie - przenosimy na koniec porządku iteracji.
  if (this->contains(k)) {
    return insert_if_exist(k);
  }

  // Nie ma takiego klucza w mapie.
  return insert_if_does_not_exist(k, v);
}

template<class K, class V, class Hash>
bool insertion_ordered_map<K, V, Hash>::insert_if_exist(const K &k) {

  _list_K_V_iterator list_it_to_edit = (*map_ptr)[k];
  V second_to_insert = list_it_to_edit->second;
  list_ptr->erase(list_it_to_edit);

  try {
    list_ptr->push_back({k, second_to_insert});
  } catch (std::bad_alloc& e) {
    throw lookup_error();
  }

  try {
    insert_last_elem_to_map();
  } catch (std::bad_alloc& e) {
    list_ptr->pop_back();
    throw lookup_error();
  }

  return false;
}

template<class K, class V, class Hash>
bool insertion_ordered_map<K, V, Hash>::insert_if_does_not_exist(const K &k,
  const V &v) {
  try {
    list_ptr->push_back({k, v});
  } catch (std::bad_alloc& e) {
    throw lookup_error();
  }
  try {
    insert_last_elem_to_map();
  } catch (std::bad_alloc& e) {
    list_ptr->pop_back();
    throw lookup_error();
  }
  return true;
}

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::insert_last_elem_to_map() {
  _list_K_V_iterator list_it_to_insert = get_last_list_iterator();
  map_ptr->insert({list_it_to_insert->first, list_it_to_insert});
}

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::erase(const K &k) {
  this->copy();

  auto iter = map_ptr->find(k);
  if (iter != map_ptr->end()) {
    _list_K_V_iterator iterator_to_erase;

    try {
      iterator_to_erase = (*map_ptr)[k];
    } catch (std::bad_alloc& e) {
      throw lookup_error();
    }

    list_ptr->erase(iterator_to_erase);
    map_ptr->erase(iter);
  } else {
    throw lookup_error();
  }
}

// Do testowania
template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::printList() {
  typename std::list<std::pair<K, V>>::iterator it = list_ptr->begin();
  while (it != list_ptr->end()) {
    std::cout << it->first << std::endl;
    ++it;
  }
  std::cout << "Koniec" << std::endl;
}

template<class K, class V, class Hash>
size_t insertion_ordered_map<K, V, Hash>::size() const {
  return list_ptr->size();
}

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::clear() {
  list_ptr = std::make_shared<_list_K_V>();
  map_ptr = std::make_shared<_map_K_iterator>();
}

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::merge(
  insertion_ordered_map const &other) {
  this->copy();

  _map_K_iterator map_to_insert = _map_K_iterator();
  _list_K_V list_to_insert = _list_K_V();

  insert_this_map_to_new_map(map_to_insert, list_to_insert);
  insert_other_map_to_new_map(other, map_to_insert, list_to_insert);

  this->map_ptr = std::make_shared<_map_K_iterator>(map_to_insert);
  this->list_ptr = std::make_shared<_list_K_V>(list_to_insert);
//  this->ref_given = std::make_shared<bool>(false);
}

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::insert_this_map_to_new_map(
  insertion_ordered_map::_map_K_iterator &map_to_insert,
  insertion_ordered_map::_list_K_V &list_to_insert) const {

  for (_list_K_V_iterator list_it = list_ptr->begin();
    list_it != list_ptr->end(); list_it++) {
    try {
      list_to_insert.push_back(*list_it);
    } catch (std::bad_alloc& e) {
      throw lookup_error();
    }
    try {
      _list_K_V_iterator list_it_to_insert = list_to_insert.end();
      list_it_to_insert--;

      map_to_insert.insert({list_it_to_insert->first, list_it_to_insert});
    } catch (std::bad_alloc& e) {
      list_ptr->pop_back();
      throw lookup_error();
    }
  }
}

template<class K, class V, class Hash>
void insertion_ordered_map<K, V, Hash>::insert_other_map_to_new_map(
  const insertion_ordered_map &other,
  insertion_ordered_map::_map_K_iterator &map_to_insert,
  insertion_ordered_map::_list_K_V &list_to_insert) const {
  for (_list_K_V_iterator list_it = other.list_ptr->begin();
    list_it != other.list_ptr->end(); list_it++) {

    if (map_to_insert.count(list_it->first) == 0) {
      try {
        list_to_insert.push_back(*list_it);
      } catch (std::bad_alloc& e) {
        throw lookup_error();
      }
      try {
        _list_K_V_iterator list_it_to_insert = list_to_insert.end();
        list_it_to_insert--;

        map_to_insert.insert({list_it_to_insert->first, list_it_to_insert});
      } catch (std::bad_alloc& e) {
        throw lookup_error();
      }
    } else {
      _list_K_V_iterator list_it_to_edit = map_to_insert[list_it->first];
      K key_to_insert = list_it->first;
      V value_to_insert = list_it_to_edit->second;
      list_to_insert.erase(list_it_to_edit);

      try {
        list_to_insert.push_back({key_to_insert, value_to_insert});
      } catch (std::bad_alloc& e) {
        throw lookup_error();
      }

      try {
        _list_K_V_iterator list_it_to_insert = list_to_insert.end();
        list_it_to_insert--;

        map_to_insert[list_it_to_insert->first] = list_it_to_insert;
      } catch (std::bad_alloc& e) {
        throw lookup_error();
      }
    }
  }
}

template<class K, class V, class Hash>
V &insertion_ordered_map<K, V, Hash>::at(const K &k) {
  this->copy();
  this->ref_given = std::make_shared<bool>(true);

  if (contains(k)) {
    return get_element(k);
  } else {
    throw lookup_error();
  }
}

template<class K, class V, class Hash>
V const &insertion_ordered_map<K, V, Hash>::at(const K &k) const {
  if (contains(k)) {
    return get_element(k);
  } else {
    throw lookup_error();
  }
}

template<class K, class V, class Hash>
V &insertion_ordered_map<K, V, Hash>::operator[](const K &k) {
  this->copy();
  this->ref_given = std::make_shared<bool>(true);

  if (!contains(k)) {
    this->insert(k, V());
  }

  return this->get_element(k);
}

template<class K, class V, class Hash>
bool insertion_ordered_map<K, V, Hash>::contains(const K &k) const {
  return map_ptr->count(k) > 0;
}

template<class K, class V, class Hash>
bool insertion_ordered_map<K, V, Hash>::empty() const {
  return list_ptr->empty();
}


#endif //INSERTION_ORDERED_MAP_H