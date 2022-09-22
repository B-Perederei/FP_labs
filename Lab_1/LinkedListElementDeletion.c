#include <stdio.h>
#include <stdlib.h>

unsigned long long input_element_list(char* text) 
{
    int problem_status;
    unsigned long long integer_back;
    char s, ch;
    char* end;
    char* users_input = calloc(25, sizeof(char));
    while(1)
    {
        printf("%s", text);
        problem_status = scanf("%s%c", users_input, &s);
        if(problem_status != 2 || s != '\n')
        {
            printf("Not valid input, try again!\n");
            while ((ch = getchar()) != '\n' && ch != EOF); 
            continue;
        }
        integer_back = strtol(users_input, &end, 0);
        if(*end == '\0')
        {
            if(integer_back > 999999999999999 || integer_back < 1)
            {
              printf("Not valid number, try again!\n");
              continue;
            }
            free(users_input);
            return integer_back;
        }
        else
        {
            if (users_input[0] == '$') {
                return 0;
            }
            printf("Not valid input, try again!\n");
        }   
    }
}

unsigned long long input_limit(char* text) 
{
    int problem_status;
    unsigned long long integer_back;
    char s, ch;
    char* end;
    char* users_input = calloc(25, sizeof(char));
    while(1)
    {
        printf("%s", text);
        problem_status = scanf("%s%c", users_input, &s);
        if(problem_status != 2 || s != '\n')
        {
            printf("Not valid input, try again!\n");
            while ((ch = getchar()) != '\n' && ch != EOF); 
            continue;
        }
        integer_back = strtol(users_input, &end, 0);
        if(*end == '\0')
        {
            if(integer_back > 999999999999999 || integer_back < 0)
            {
              printf("Not valid number, try again!\n");
              continue;
            }
            free(users_input);
            return integer_back;
        }
        else
        {
            printf("Not valid input, try again!\n");
        }   
    }
}

typedef struct node
{
  unsigned long long number;
  struct node* next;
} node;

struct node* create(void)
{
  node* head = NULL;
  node *current, *previous;
  int index_first_element = 0;
  int switch_input = 0;
  printf("Enter '$' to exit adding new elements to list.\n");
  while(1)
  {
    current = (node*) malloc(sizeof(node));
    current->number = input_element_list("Enter natural number: ");
    // Exiting from entering numbers
    if (current->number == 0) {
        free(current);
        if (head != NULL) {
            break;
        }
        printf("List can't be empty!\n");
        continue;
    }

    if(index_first_element == 0)
    {
      // First writing to head
      head = previous = current;
      index_first_element++;
    }
    if(previous)
    {
      previous -> next = current;
    }
    previous = current;
  }
  previous->next = NULL;
  return head;
}

void free_list(node* head)
{
  node *current;
  while(head)
  {
    current = head;
    head = head->next;
    free(current);
  } 
}

void show_list(node* head)
{ 
  node* current = head;
  while(current)
  {
    printf("%llu ", current->number);
    current = current->next;
  }
  printf("\n\n");
}

struct node* delete_numbers(node* head, unsigned long long lower_limit, unsigned long long upper_limit) {
   node* current = head;
   node* previous = NULL;
   while(current) {
       if (current->number <= lower_limit || current->number >= upper_limit) {
           node* to_free = current;
           current = current->next;
           free(to_free);
           if (previous == NULL) {
               // If need to delete head
                head = current;
           }
           else {
                previous->next = current;
           }
       }
       else {
            previous = current;
            current = current->next;
       }
   }
   return head;
}

int main(void) {
    unsigned long long choice;
    while(1)
    {
        printf("Programs available: \n");
        printf("1) Creating list of numbers and delete numbers which is NOT in given interval;\n");
        printf("2) Close the program.\n");
        printf("\n");
        while(1)
        {
            choice = input_limit("Enter number of program: ");
            if(choice == 1 || choice == 2)
            {
                printf("\n");
                break;
            }
            else
            {
              printf("Not valid input. Please try again!\n");
            }
        }
        if(choice == 1)
        {
            // Main program here
            node* head = create();
            printf("Entered list: ");
            show_list(head);
            unsigned long long lower_limit = input_limit("Enter lower limit: ");
            unsigned long long upper_limit = 0;
            while (lower_limit >= upper_limit) {
                upper_limit = input_limit("Enter upper limit: ");
                if (lower_limit >= upper_limit) {
                printf("Upper limit should be bigger then lower limit.\n");
                }
            }
            head = delete_numbers(head, lower_limit, upper_limit);
            printf("Result list: ");
            show_list(head);
            free_list(head);
        }
        else
        {
          printf("Thank you for using program!\n");
          break;
        }
    }
  return 0;
}