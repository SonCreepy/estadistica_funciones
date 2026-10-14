//
// Pepe Gallardo, Data Structures, UMA.
//

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "LinkedStructure.h"


/// @brief Constructor for a new Node
/// @param element Element to be stored in node
/// @param p_next Pointer to next to be stored in node
/// @return Pointer to new node
static struct Node* Node_new(int element, struct Node* p_next) {
  struct Node* p_node = malloc(sizeof(struct Node));
  assert(p_node != NULL && "Node_new: not enough memory");

  p_node->element = element;
  p_node->p_next = p_next;
  return p_node;
}

/// @brief Destructor for a Node. Frees memory allocated for node and sets pointer to node to NULL
/// @param p_p_node Pointer to pointer to node to be freed 
static void Node_free(struct Node** p_p_node) {
  free(*p_p_node);
  *p_p_node = NULL;
}

struct Node* LinkedStructure_new() {
  return NULL;
}

struct Node* LinkedStructure_copyOf(const struct Node* p_first) {
  struct Node* p_node;
  if(LinkedStructure_isEmpty(p_first)){
    p_node = NULL;
  }else{
    while(p_first ->p_next != NULL){
      p_node = p_first;
      p_first = p_first ->p_next;
    }
  }
  return p_node;
}

bool LinkedStructure_isEmpty(const struct Node* p_first) {
  return p_first == NULL;
}

size_t LinkedStructure_size(const struct Node* p_first) {
  size_t size = 0;
  struct Node* p_node = p_first;
  while(p_node != NULL){
      p_node = p_node ->p_next;
      size++;
  }
  return size;
}

void LinkedStructure_clear(struct Node** p_p_first) {
   assert(p_p_first != NULL && "LinkedStructure_clear: null pointer sent as parameter");

  if(!LinkedStructure_isEmpty(*p_p_first)){
    struct Node* p_current = *p_p_first;
    while(p_current != NULL){
      struct Node* p_previous = p_current;
      Node_free(&p_previous);
      p_current = p_current ->p_next;
    }
    p_p_first = NULL;
  }
}

void LinkedStructure_prepend(struct Node** p_p_first, int element) {
  assert(p_p_first != NULL && "LinkedStructure_prepend: null pointer sent as parameter");
  struct Node* p_first = *p_p_first;
  if(LinkedStructure_isEmpty(p_first)){
    struct Node* p_node = Node_new(element,NULL);
    p_first ->p_next = p_node;
  }else{
    struct Node* p_node = Node_new(element,p_first ->p_next);
    p_first ->p_next = p_node;
  }
}

void LinkedStructure_append(struct Node** p_p_first, int element) {
  assert(p_p_first != NULL && "LinkedStructure_append: null pointer sent as parameter");
  struct Node* p_last = *p_p_first;  
  if(LinkedStructure_isEmpty(p_last)){
    LinkedStructure_prepend(p_p_first,element);
  }else{
    while(p_last ->p_next !=NULL){
      p_last = p_last ->p_next;
    }
    struct Node* p_node =Node_new(element,NULL);
    p_last -> p_next = p_node;
  } 
}

void LinkedStructure_insert(struct Node** p_p_first, size_t index, int element) {
  assert(p_p_first != NULL && "LinkedStructure_insert: null pointer sent as parameter");
  assert(LinkedStructure_size(*p_p_first) >= index && "LinkedStructure_insert: index sent is greater than list size"); 
  if(index == 0){
    LinkedStructure_prepend(p_p_first,element);
  }else{
    struct Node* p_current = *p_p_first;
    struct Node* p_previous = NULL;
    for(size_t i = 0; i < index;i++){
        p_previous = p_current;
        p_current = p_current ->p_next;
      }
        struct Node* p_node = Node_new(element,p_current);
        p_previous ->p_next = p_node;  
    } 
}

int LinkedStructure_get(const struct Node* p_first, size_t index) {
  assert(p_first != NULL && "LinkedStructure_get: null pointer sent as parameter");
  assert(LinkedStructure_isEmpty(p_first) && "LinkedStructure_get: empty list sent as parameter");
  assert(LinkedStructure_size(p_first) >= index && "LinkedStructure_get: index sent is greater than list size"); 

  int r = 0;
    struct Node* p_node = p_first;
    for(size_t i = 0; i < index;i++)
      p_node = p_node ->p_next;
    r = p_node ->element; 
  return r;
}

void LinkedStructure_set(struct Node* p_first, size_t index, int element) {
  assert(LinkedStructure_isEmpty(p_first) && "LinkedStructure_set: empty list sent as parameter");
  assert(LinkedStructure_size(p_first) >= index && "LinkedStructure_set: index sent is greater than list size");  
  struct Node* p_node = p_first;
    for(size_t i = 0; i < index;i++)
      p_node = p_node ->p_next;
    p_node ->element = element; 
}

void LinkedStructure_delete(struct Node** p_p_first, size_t index) {
  assert(LinkedStructure_isEmpty(*p_p_first) && "LinkedStructure_delete: empty list sent as parameter");
  assert(LinkedStructure_size(*p_p_first) >= index && "LinkedStructure_delete: index sent is greater than list size");  

  struct Node* p_current = *p_p_first;
  struct Node* p_previous = NULL;
  for(size_t i = 0; i < index;i++){
    p_previous = p_current;
    p_current = p_current ->p_next;
  }
  p_previous ->p_next = p_current ->p_next;
  Node_free(&p_current); 
}

void LinkedStructure_print(const struct Node* p_first) {
  printf("LinkedStructure(");
  for (const struct Node* p = p_first; p != NULL; p = p->p_next) {
    printf("%d", p->element);
    if (p->p_next != NULL) {
      printf(", ");
    }
  }
  printf(")");
}

