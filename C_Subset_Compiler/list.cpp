     //Aniruddha Kanhere   200261603   arkanher@ncsu.edu

#include "list.h"
#include "stdlib.h"

paramlist_t* push_param(paramlist_t *head, const char *name, Type* type)
{
  paramlist_t * node = (paramlist_t*) malloc(sizeof(paramlist_t));
  node->next = head;
  node->name = name;
  node->type = type;
  return node;
}

paramlist_t* pop_param(paramlist_t *head)
{
  paramlist_t *next = head->next;
  free(head);
  return next;
}

paramlist_t* next_param(paramlist_t *head)
{
  paramlist_t *next = head->next;
  return next;
}

int paramlist_size(paramlist_t *head)
{
  int cnt=0;
  paramlist_t *temp = head;
  while(temp)
    {
      cnt++;
      temp=temp->next;
    }
  
  return cnt;
}

typedef struct loop_list {
  loop_info_t info;
  struct loop_list *next;
} loop_list_t;

typedef struct if_list {
  if_info_t if_info;
  struct if_list *next;
} if_list_t;

typedef struct switch_list {
  switch_info_t switch_info;
  struct switch_list *next;
} switch_list_t;


static loop_list_t *head = NULL;
static if_list_t *if_head = NULL;
static switch_list_t *switch_head = NULL;

void push_loop(BasicBlock* expr,
	       BasicBlock* body,
	       BasicBlock* reinit,
	       BasicBlock* exit)
{
  loop_list_t *n = (loop_list_t*) malloc(sizeof(loop_list_t));
  n->info.expr = expr;
  n->info.body = body;
  n->info.reinit = reinit;
  n->info.exit = exit;

  n->next = head;
  head = n;
}

void pop_loop()
{
  loop_list_t *tmp = head;
  head = head->next;
  free(tmp);
}

loop_info_t get_loop()
{
  loop_info_t n;
  n.expr = n.body = n.reinit = n.exit = NULL;
  
  if (head)
    return head->info;
  else
    return n;
}


void push_if(BasicBlock* then,
	       BasicBlock* els,
	       BasicBlock* join)
{
  if_list_t *n = (if_list_t*) malloc(sizeof(if_list_t));
  n->if_info.then = then;
  n->if_info.els = els;
  n->if_info.join = join;

  n->next = if_head;
  if_head = n;
}


void pop_if()
{
  if_list_t *tmp = if_head;
  if_head = if_head->next;
  free(tmp);
}

if_info_t get_if()
{
  if_info_t n;
  n.then = n.els = n.join = NULL;
  
  if (if_head)
    return if_head->if_info;
  else
    return n;
}


void push_switch(BasicBlock* default_case,
	         SwitchInst *switch_body,
                 BasicBlock *last_case,
                 int broken              )
{
  switch_list_t *n = (switch_list_t*) malloc(sizeof(switch_list_t));
  n->switch_info.default_case = default_case;
  n->switch_info.switch_body = switch_body;
  n->switch_info.last_case = last_case;
  n->switch_info.broken = broken;
  n->next = switch_head;
  switch_head = n;
}


void pop_switch()
{
  switch_list_t *tmp = switch_head;
  switch_head = switch_head->next;
  free(tmp);
}

switch_info_t get_switch()
{
  switch_info_t n;
  n.default_case = NULL;
  n.switch_body  = NULL;
  n.last_case = NULL;
  n.broken = 0;
  
  if (switch_head)
    return switch_head->switch_info;
  else
    return n;
}
