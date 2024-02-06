#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

/* C does not have extensible vectors like more modern languages, which is
 * rather annoying.  So instead, we use a home-grown linked list
 * implementation to provide similar functionality.
 */

typedef struct node {
    int key;
    void *value;
    struct node *next;
} node_t;

void append_list(node_t **list, int key, void *value) {
    node_t *curr = *list;
    node_t *temp  = (node_t *)malloc(sizeof(node_t));
    temp->key = key;
    temp->value = value;
    temp->next = NULL;
    if ( *list == NULL ) {
        *list = temp;
    } else {
        while ( curr->next != NULL ) {
            curr = curr->next;
        }
        curr->next = temp;
    }
}

void *search_list(node_t *list, int key) {
    node_t *listptr = list;
    while ( listptr != NULL ) {
        if ( listptr->key == key ) {
            return listptr->value;
        }
        listptr = listptr->next;
    }
    return NULL;
}

void free_list(node_t *list) {
    node_t *listptr = list;
    node_t *temp;
    while ( listptr != NULL ) {
        temp = listptr;
        listptr = listptr->next;
        free(temp->value);
        free(temp);
    }
}

void *update_list(node_t *list, int key, void *new_val) {
    node_t *curr = list;
    while ( curr != NULL ) {
        if ( curr->key == key ) {
            break;
        } else {
            curr = curr->next;
        }
    }
    if ( curr != NULL ) {
        free(curr->value);
        curr->value = new_val;
    }
}

void split_once(const char *source, char delim, char **first, char **second) {
    char *p = NULL;
    int pos = 0;

    if ( (p = strchr(source, delim)) != NULL ) {
        pos = p - source;
        if ( first != NULL ) {
            *first = strndup(source, pos);
        }
        if ( second != NULL ) {
            *second = strndup(source + pos + 1, strlen(source) - pos);
        }
    } else {
        if ( first != NULL ) {
            *first = NULL;
        }
        if ( second != NULL ) {
            *second = NULL;
        }
    }
}

void scan_numbers(const char *s, node_t **list) {
    size_t pos = 0;
    size_t start_pos;
    char *num_str;
    int num;
    int scanning_number = 0;
    int *val;

    for ( ; ; ) {
        if ( scanning_number ) {
            if ( !isdigit(s[pos]) ) {
                num_str = strndup(s + start_pos, pos - start_pos);
                num = atoi(num_str);
                val = (int *)malloc(sizeof(int));
                *val = 1;
                append_list(list, num, val);
                scanning_number = 0;
                free(num_str);
            }
        } else {
            if ( isdigit(s[pos]) ) {
                start_pos = pos;
                scanning_number = 1;
            }
        }
        pos++;
        if ( pos == strlen(s) ) {
            break;
        }
    }
    if ( scanning_number ) {
        num_str = strndup(s + start_pos, pos - start_pos);
        num = atoi(num_str);
        val = (int *)malloc(sizeof(int));
        *val = 1;
        append_list(list, num, val);
        scanning_number = 0;
        free(num_str);
    }
}

void usage(const char *progname) {
    printf("usage: %s <file>\n", progname);
    exit(EXIT_FAILURE);
}

uint32_t process(const char *filename) {
    uint32_t result = 0;
    char *line = NULL;
    FILE *infile = NULL;
    size_t len = 0;
    ssize_t nread = 0;
    char *card_part, *card_num_str, *rest, *winning_str, *hand_str;
    node_t *winning_set;
    node_t *hand_set;
    node_t *nodeptr;
    int card_num;
    int common_count;
    int i, copies;
    node_t *instances = NULL;
    int *val, *new_val;
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }
    while ( (nread = getline(&line, &len, infile)) != -1 ) {
        len = strlen(line);
        line[len - 1] = '\0';
        split_once(line, ':', &card_part, &rest);
        split_once(card_part, ' ', NULL, &card_num_str);
        card_num = atoi(card_num_str);
        split_once(rest, '|', &winning_str, &hand_str);
        winning_set = NULL;
        hand_set = NULL;
        scan_numbers(winning_str, &winning_set);
        scan_numbers(hand_str, &hand_set);
        nodeptr = winning_set;
        common_count = 0;
        while ( nodeptr != NULL ) {
            if ( search_list(hand_set, nodeptr->key) != NULL ) {
                common_count++;
            }
            nodeptr = nodeptr->next;
        }
        for ( i = card_num + 1; i <= card_num + common_count; i++ ) {
            copies = 0;
            val = (int *)search_list(instances, i);
            if ( val != NULL ) {
                copies += *val;
            }
            copies++;
            val = (int *)search_list(instances, card_num);
            if ( val != NULL ) {
                copies += *val;
            }
            new_val = (int *)malloc(sizeof(int));
            *new_val = copies;
            if ( search_list(instances, i) != NULL ) {
                update_list(instances, i, new_val);
            } else {
                append_list(&instances, i, new_val);
            }
        }
        result++;
    }
    fclose(infile);
    nodeptr = instances;
    while ( nodeptr != NULL ) {
        val = (int *)nodeptr->value;
        result += *val;
        nodeptr = nodeptr->next;
    }
    free_list(winning_set);
    free_list(hand_set);
    free_list(instances);
    return result;
}

int main(int argc, char *argv[]) {
    char *filename = NULL;
    uint32_t result = 0;

    if ( argc < 2 ) {
        usage(argv[0]);
    }
    filename = argv[1];
    result = process(filename);
    printf("result = %d\n", result);
    return EXIT_SUCCESS;
}
